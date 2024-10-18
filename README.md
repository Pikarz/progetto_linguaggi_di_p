# Simulazione di un Mini-Linguaggio con Puntatori e Bucket
**Introduzione**

Questo progetto simula un mini-linguaggio che gestisce la memoria tramite puntatori, utilizzando l'approccio dei bucket per ottimizzare la gestione. Una funzione hash viene impiegata per associare in modo univoco un nome a un determinato bucket. Gli elementi vengono inseriti in coda ai bucket man mano che vengono inizializzati. Il sistema definisce tre ambienti principali:

    Ambiente dei Nomi (En):
    Una lista di liste della lunghezza di n, definita inizialmente dalla costante const_ncells. Le liste più interne, chiamate bucket, hanno lunghezza variabile in base al numero di elementi che sono stati inizializzati in quel momento e assegnati al bucket tramite la funzione hash. L'ambiente En contiene i nomi delle variabili inizializzate dall'utente, ciascuna delle quali ha un puntatore associato.

    Ambiente dei Puntatori (Ep):
    Una lista di liste speculare a En, dove ogni puntatore è associato univocamente a una variabile in En. In particolare, in Ep[i,j] si trova il puntatore che fa riferimento alla variabile En[i,j]. I puntatori sono rappresentati come interi e il puntatore p punta alla zona di memoria V[p] dell'ambiente dei valori V.

    Ambiente dei Valori (V):
    Un array monodimensionale di lunghezza n², che memorizza i valori associati alle variabili. Ogni puntatore fa riferimento a un'area specifica della memoria in questo array.

Durante l'esecuzione del programma, per un'ottimizzazione in termini di efficienza, viene anche mantenuta una lista dei puntatori liberi, chiamata P, che rappresenta i puntatori disponibili in un dato momento.

**Linguaggio Utilizzato**

Il progetto è stato implementato utilizzando Standard ML (SML), un linguaggio di programmazione funzionale, modulare e di alto livello. SML offre controllo statico dei tipi e inferenza dei tipi a tempo di compilazione, rendendolo adatto a implementazioni che richiedono precisione e gestione della memoria.
