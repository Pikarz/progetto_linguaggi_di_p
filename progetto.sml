(* definisco la variabile che corrisponderà al numero di buckets della memoria *)
	val const_ncells = 3;

(* definisco tipi custom che saranno utilizzati *)
	datatype types = 
		Str of string | 
		Int of int | 
		IntList of (int list) | 
		StrList of (string list) | 
		Bool of bool | 
		Null of unit;

(* funzione HASH *)
	(* charListToInt prende una stringa y, la trasforma in una lista di caratteri con explode, mappando ogni carattere della lista con Char.ord, una funzione che converte un carattere nell'intero corrispondente ASCII *)
    	fun charListToInt (y) = map (fn x => Char.ord (x)) (explode(y));

	(* sumList e sumListSupp prendono una lista di interi e restituiscono la somma di ogni elemento.
	sumListSupp fa uso di 'tl' che, data una lista, restituisce la lista meno il primo elemento. La funzione usa anche 'hd' che fa pop del primo elemento della lista. Il caso base avviene quando la lista è vuota, restituendo il contatore. sumList chiama semplicemente sumListSupp con il contatore inizializzato a 0. *)
	    fun sumListSupp([], n) = n |
	    sumListSupp(x, m) = sumListSupp((tl x), (m + hd x));

	    fun sumList(x) = sumListSupp(x, 0);


	(* hash dato un n restituisce la classe resto modulo n della somma dei caratteri ascii della stringa data in input. Nel nostro caso n sarà uguale al numero di righe della matrice *)
	    fun hash(x, n) = (sumList (charListToInt x)) mod n;

(* gestione ambienti *)

	(* la funzione getBucket() aggiunge ricorsivamente elementi di un bucket vecchio l in un bucket nuovo x.
	oss: il caso base è lo stesso di sumListSupp (false di Church) *)
		fun getBucket([], x) = x
		    | getBucket(l, x) =
		        getBucket( (tl l), 
		        x @ ((hd l)::[]));

    (* addToBucket() appende a un bucket specifico l'elemento x in coda. *)
		fun addToBucket(l, x) = getBucket(l, []) @ x::[];

	(* newPointer prende l'array dei puntatori liberi e ne restituisce uno con la lista aggiornata dei puntatori liberi *)
		exception MemoryFullException;

		fun newPointer(P: int list) = 
			if P=[] then raise MemoryFullException
			else (tl P, hd P);
	(* searchBucket, dato un bucket e una certa variabile da cercare, restituisce l'indice j dove si trova la variabile in b. Se x non esiste in b allora lancia un'eccezione *)
		exception VariableDoesNotExistsException
		fun searchBucket(b: string list, x, j) =
			if (List.null b) then raise VariableDoesNotExistsException
			else if ((hd b)=x) then j
			else searchBucket((tl b), x, j+1);

	(* editSingleCell, dato un ambiente, un valore e una coppia di coordinate i,j, setta la posizione dell'ambiente i,j a v*)
		fun editSingleCell(E : 'a list list, v, i, j) =
			let val newbucket = List.take( (List.nth(E, i)) , j) @ v::[] @ (List.drop( (List.nth(E, i)), j+1)) in
				List.take(E, i) @ newbucket::[] @ List.drop(E, i+1)
			end;

	(*getCoordinates dato un ambiente dei nomi e una x, restituisce la posizione dove è presente x in En *)
		fun getCoordinates(En, x) = 
			let val i = hash(x, const_ncells) in
				(i, searchBucket(List.nth(En, i), x, 0))
			end;

	(* refer dati gli ambiente dei nomi e dei puntatori e un x, restituisce il puntatore associato a x *)	
		fun refer(En, Ep, x) =
			let val (i, j) = getCoordinates(En, x) in
				List.nth(List.nth(Ep, i), j)
			end;

	(* changeEnvBucket sostituisce il bucket in E[i] con b *)
		fun changeEnvBucket(E, b, i) = 
			List.take(E, i) @ b::[] @ List.drop(E, i+1);

	(* updateEnvs a partire da un ambiente dei nomi vecchio En, un nome x, un ambiente dei puntatori vecchio Ep e la lista dei puntatori "liberi" P, aggiorna i due ambienti (nomi e puntatori) inserendo la nuova variabile x nell'apposito bucket, restituendo anche la lista aggiornata dei puntatori ancora liberi e l'ultimo puntatore della variabile appena aggiunta. *)
		fun updateEnvs(En, x, Ep, P) = 
			(let val (i, j) = getCoordinates(En, x) in (editSingleCell(En, x, i, j), Ep, P, refer(En, Ep, x)) end)
				handle VariableDoesNotExistException =>
					let val i = hash(x, const_ncells) in
						let val (Pn, p) = newPointer(P) in
							(changeEnvBucket(En, (List.nth(En, i) @ x::[]), i), changeEnvBucket(Ep, (List.nth(Ep, i) @ p::[]), i), Pn, p)
						end
					end;

	(* addName, dato un ambiente dei nomi En, un ambiente dei puntatori Ep, la lista dei puntatori ancora liberi e un nome di variabile x, assegna x all'ultima posizione della lista di indice definito dalla funzione hash in En e, in modo speculare, gli assegna un puntatore in Ep. *)
		fun addName(En, Ep, P, x) = (updateEnvs(En, x, Ep, P));

	(* changePointedValue è la funzione analoga di addName per l'ambiente dei valori. Permette anche l'assegnamento di valori attraverso puntatori *)
		fun changePointedValue(V, v, p) = 
			List.take(V, p) @ v::[] @ List.drop(V, p+1);

	(* la funzione assign, dato un ambiente dei nomi En, un ambiente dei puntatori Ep, un ambiente delle variabili V, una variabile x e un valore v, aggiunge x a En e associa a x un puntatore p di Ep che punta alla zona di memoria V[p] alla quale associa v *)
		(* creo un eccezione che viene lanciata quando non c'è memoria *)
		exception MemoryFullException;
		fun assign(En, Ep, P, V, x, v) =
			if P = [] then
				raise MemoryFullException
			else
				let val (Enn, Epn, Pn, p) = addName(En, Ep, P, x) in
					(Enn, Epn, Pn, changePointedValue(V, v, p))
				end;

	(* newLocation è la funzione di supporto di initCompiler() e aggiunge ricorsivamente i bucket vuoti dei nomi/puntatori a E *)
		fun newLocation(0, E) = E |
			newLocation(n, E) = newLocation(n-1, E @ [[]]);

	(* initPointers è un'altra funzione di supporto di initCompiler() che crea l'array dei puntatori liberi *)
		fun initPointers(P, 0) = P |
		initPointers(P, n) = initPointers(P @ n-1::[], n-1);

	(* initValues è un'altra funzione di supporto di initCompiler() che crea l'array dei valori *)
		(* fun initValues(V, 0) = V @ "-"::[] |
		initValues(V, n) = initValues(V @ "-"::[], n-1); *)
		fun initValues(V, 0) = V |
			initValues(V, n) = initValues(V @ Null()::[], n-1);


	(* initCompiler è la funzione che inizializza l'ambiente dei nomi, l'ambiente dei puntatori e la lista di puntatori liberi. Prende come input un n creando due matrici n*0 di celle e un array dei puntatori lungo n^2. Per ottimizzare l'uso della memoria, inizialmente i bucket sono vuoti (per cui lo 0), poi man mano che l'utente inizializza e fa cose vengono riempiti. Il tipo va specificato perché altrimenti mi esce un qualcosa di tipo "?.X1 list list" che non è compatibile con "string list list" *)
		fun initCompiler(const_ncells) : (string list list * int list list * int list * types list) = (newLocation(const_ncells,[]), newLocation(const_ncells,[]), initPointers([], (const_ncells*const_ncells)), initValues([], (const_ncells*const_ncells)));
		
	(* assignPointer dato un ambiente dei nomi, un ambiente dei puntatori, una x e un puntatore, assegna alla x il puntatore *)
	fun assignPointer(En : string list list, Ep : int list list, x : string, p: int) = 
		let val (i,j) = getCoordinates(En, x) in
			editSingleCell(Ep, p, i, j)
		end;

	(* remove dato un ambiente e delle coordinate, rimuove il nome/puntatore E[i,j] da E *)
		fun remove(E, i, j) = 
			let val newbucket = (List.take(List.nth(E, i), j)) @ (List.drop((List.nth(E, i),j+1))) in
				List.take(E, i) @ newbucket::[] @ List.drop(E, i+1)
			end;

	(* free è libera la memoria assegnata a x. Crea garbage non pulendo il valore in V assegnato a x *)
		fun free(En, Ep, P, x) = 
			let val p = refer(En, Ep, x) in
				let val (i,j) = getCoordinates(En, x) in 
					( remove(En, i, j), remove(Ep, i, j), (P @ p::[]))
				end
			end;

	(* la funzione isRef si aspetta in input l'ambiente dei puntatori Ep e un certo p. La funzione restituisce true se p è in Ep, false altrimenti *)
		fun isRef([], p) = false |
		isRef(Ep : int list list, p) = 
			let val head = (hd Ep) in
				if (head=[]) then isRef(List.drop(Ep, 1), p)
				else if (hd (head)) = p then true
				else isRef( (remove(Ep, 0, 0), p))
			end;

	(* changePointer modifica il puntatore associato a x in p *)
		fun changePointer(En, Ep, P, V, p, x) = 
			let val (i,j) = getCoordinates(En, x) in 
				(En, editSingleCell(Ep, p, i, j), P, V)
			end;

	(* il garbage collector rimuove tutti i valori inizializzati in V ma non referenziati da alcun puntatore *)
		fun garbageCollector(Ep, [], V_new) = V_new |
			garbageCollector(Ep, V, V_new) =
				let val newvalue = hd V in
					if (newvalue=Null()) then
						garbageCollector(Ep, tl V, V_new @ Null()::[])
					else
						if (isRef(Ep, (length V_new))) then
							garbageCollector(Ep, tl V, V_new @ newvalue::[])
						else
							garbageCollector(Ep, tl V, V_new @ Null()::[])
				end;


(* valutazione di variabili *)
	(* defer, dato un puntatore e l'ambiente dei valori, restituisce il valore puntato da p in V *)
		fun defer(V, p: int) = 
			List.nth(V, p);

	(* getBucketCell, dato un ambiente dei puntatori, un ambiente delle variabili e le coordinate i,j, restituisce il valore V[p] dove p = il puntatore in pozione [i,j] nell'ambiente Ep *)
		fun getBucketCell(Ep : int list list, V, i, j) = 
			defer(V, (List.nth(List.nth(Ep, i), j)));

	(* valIt dato un ambiente dei nomi, un ambiente dei puntatori, un ambiente delle variabili e una certa x, valuta x in En, estraendone il suo puntatore e riportando il valore in V corrispettivo a x grazie a esso.
	*)
		fun valIt(En, Ep, V, x) =
			let val (i,j) = getCoordinates(En, x) in
				getBucketCell(Ep, V, i, j)
			end;

(* valutazione e funzioni dei tipi custom *)

	fun intVal(Int i) = i;
	fun strVal(Str s) = s;
	fun boolVal(Bool b) = b;
	fun intListVal(IntList i) = i;
	fun strListVal(StrList i) = i;
		
	(* concat e somma *)
	fun concatString [] = ""
	  | concatString (x::xs) = strVal x ^ concatString xs;

	 fun sumInt [] = 0 |
	 	sumInt (x::xs) = intVal x + sumInt xs; 
