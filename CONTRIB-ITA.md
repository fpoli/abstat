Come contribuire al progetto
============================


Requisiti
---------

Per poter lavorare al progetto è necessario:
- conoscere la teoria dell'[interpretazione astratta](https://en.wikipedia.org/wiki/Abstract_interpretation) e la relativa terminologia;
- saper programmare in [Haskell](https://www.haskell.org/) (soprattutto saper usare le classi di Haskell).


Documentazione
--------------

Il codice del progetto non è ricco di commenti, ma la segnatura dei tipi unitamente al nome delle funzioni, delle classi e delle variabili dovrebbe richiamare alla mente le relative definizioni dell'interpretazione astratta.

Ad esempio, in `src/While/Concrete.hs`:
```haskell
type Domain = Integer

-- ...

interpretStmt :: Stmt -> State Domain -> State Domain
interpretStmt stmt state = case stmt of
    Skip ->
        state

    (Assign var aexpr) ->
        store var val state
        where val = interpretAExpr aexpr state

    -- eccetera...
```

Il nome della funzione **`interpretStmt`** e il file **`Concrete.hs`** suggeriscono che la funzione definita è la semantica concreta del linguaggio While.

La segnatura **`Stmt -> State Domain -> State Domain`** suggerisce che la funzione riceve in input uno statement `stmt` del linguaggio While ed uno stato `state` che utilizza il dominio (concreto) degli interi. In output, restituirà nuovamente uno stato `State Domain`.
Anche l'implementazione segue la definizione teorica della sematica:
- se `stmt` è `Skip` allora restituisce lo stesso stato `state`.
- se `stmt` è una assegnazione della variabile `var` allora restituisce uno stato in cui alla variabile `var` è associato il valore dell'espressione aritmetica `aexpr`, interpretata nello stato `state`.
- eccetera...


Cartelle
--------

Nel repository sono presenti 4 cartelle:
- `data` contiene alcuni programmi di esempio scritti nel linguaggio While.
- `main` contiene `Main.hs`: lo script principale che prende in input un programma While e fornisce in output l'interpretazione astratta (e concreta) del programma. Per fare questo vengono eseguiti in successione il parser del linguaggio While e la semantica astratta dei domini specificati nello script, infine la semantica concreta (che potrebbe non terminare).
- `src` contiene la libreria di classi e funzioni del progetto. Il contenuto della cartella è illustrato successivamente.
- `test` contiene tutti i test del progetto, ordinati con la stessa struttura di cartelle usata in `src`.

### Cartella `src`

La cartella `src` contiene due sotto cartelle.

**`Abstat`** contiene tutte le funzioni e classi che non dipendono dal linguaggio di cui si sta facendo l'interpretazione astratta.
La sotto-cartella `Interface` contiene la definizione delle classi utilizzate nel progetto (`Lattice`, `Poset`, ...), mentre la sotto-cartella `Common` contiene delle funzioni di utilità che dovrebbero andar bene per diversi linguaggi (non solo per While).

**`While`** contiene tutte le definizioni specifiche per il linguaggio While. Ad esempio: il parser del linguaggio While, la semantica concreta, i domini e la semantica dei domini.


Un nuovo dominio
----------------

A grandi linee, per implementare un nuovo dominio è necessario:
 1. Creare una nuova cartella in `src/While/Domain` dando il nome del nuovo dominio. Per comodità, si può copiare e rinominare la cartella del dominio `Congruence`.
 2. Definire il dominio astratto, seguendo il modello di `src/While/Domain/Congruence/Domain.hs`.
 3. Definire la semantica astratta, seguendo il modello di `src/While/Domain/Congruence/Semantics.hs`.
 4. Aggiungere al file `main/Main.hs` l'esecuzione della nuova interpretazione astratta.

Per definire il **dominio astratto** serve:
- Definire il tipo di dato astratto `Domain`: (`data Domain = ...`).
- Definire la relazione d'ordine parziale sul dominio (`instance Lattice Domain`) con:
    - elemento `top`;
    - elemento `bottom`;
    - operazione `join` (or) tra due elementi;
    - operazione `meet` (and) tra due elementi.
- Specificare che il dominio è un dominio astratto (`instance AbstractDomain Domain where --nothing`).
- Definire l'astrazione dal dominio concreto al dominio astratto (`instance GaloisConnection Integer Domain`) con:
    - operazione `singleAbstraction`;
    - l'operazione `concretization` in genere non è definita, quindi si può lasciare la definizione `concretization _ = error "not implemented"`.
- La definizione del generatore di elementi (`instance Arbitrary Domain`) è opzionale e serve solo per l'esecuzione dei test.

Per definire la **semantica astratta** serve soltando definire un'istanza della classe `WhileAbstractSemantics`:
```haskell
instance WhileAbstractSemantics Domain where
    interpretAExpr expr state = case expr of
        (Var var) -> load var state
        -- eccetera...
```
La classe `WhileAbstractSemantics` è definita in `src/While/AbstractSemantics.hs`.

È consigliabile definire anche alcuni test per il nuovo dominio implementato: per far ciò si consiglia di prendere come esempio i test di `test/While/Domain/Congruence`.
Ovviamente i test sono utili solo quando falliscono; da soli non possono garantire la correttezza del codice.


I test
------

I test utilizzano le librerie `QuickCheck` e `Hspec`.
Nel complesso sono abbastanza leggibili: si prenda ad esempio il codice
```haskell
    describe "parseString" $ do
        it "parses simple condition" $ do
            parseString "if false then skip else skip" `shouldBe`
                If (BoolConst False) Skip Skip
```

Intuitivamente, questo test controlla che la funzione `parseString` con input `"if false then skip else skip"` restituisca l'oggetto `If (BoolConst False) Skip Skip`. Per utilizzare al meglio i test, comunque, si consiglia di leggersi un tutorial per `QuickCheck`.

I test che controllano delle proprietà, come ``a `join` b == b `join` a`` con `a` e `b` elementi di un reticolo, utilizzano dei generatori pseudo-casuali di elementi: vengono generati casualmente alcune migliaia di elementi `a` e `b`, e per ciascuna coppia viene eseguita la funzione che controlla la proprietà ``a `join` b == b `join` a``.

Le proprietà testate (commutativa, associativa, eccetera) sono definite nei file Haskell che terminano con il suffisso **`Prop.hs`**, ad esempio `src/Abstat/Interface/LatticeProp.hs`.
I "generatori di elementi" sono le implementazioni della classe **`Arbitrary`**, una classe della libreria `QuickCheck`. Ad esempio, in `src/While/Domain/Int/Domain.hs` si trova

```haskell
data Domain
    = Top
    | Val Integer
    | Bottom
    deriving (Eq,Show,Ord)

-- ...

instance Arbitrary Domain where
    arbitrary = frequency [
            (1, return Top),
            (3, liftM Val arbitrary),
            (1, return Bottom)
        ]
```

che significa, parafrasando:

> Per generare un elemento casuale del tipo Domain (cioè, del reticolo degli interi) restituisci con frequenza "1" gli elementi `Top` e `Bottom`, e con frequenza "3" un elemento casuale costruito con `Val arbitrary`. Siccome `Val` vuole un `Integer` come parametro, allora la funzione `arbitrary` genererà un numero intero casuale.


Contributi
----------

Se trovate bug o volete proporre dei miglioramenti aprite pure issue e pull request!
Probabilmente non risponderò velocemente, ma ogni contributo è sicuramente ben accetto.
