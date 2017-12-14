# TrieClean

<h1 id="trie">Trie</h1>
<p><em>A feladatok egymásra épülnek ezért a megadásuk sorrendjében kell ezeket megoldani! A függvények definíciójában lehet, sőt javasolt is alkalmazni a korábban definiált függvényeket.</em></p>
<p><em>Tekintve, hogy a tesztesetek, bár odafigyelés mellett íródnak, nem fedik le minden esetben egy-egy függvény teljes működését, ezért határozottan javasolt még külön próbálgatni a megoldásokat beadás előtt, vagy megkérdezni az oktatókat!</em></p>
<h2 id="a-feladat-rövid-leírása">A feladat rövid leírása</h2>
<p>A feladatban a <a href="https://en.wikipedia.org/wiki/Trie">Trie</a> adattípust fogjuk megvalósítani. A trie egy fa struktúra, amelyet <code>String</code> kulccsal rendelkező map (véges leképezés) adatszerkezetek implementálásra használnak leggyakrabban.</p>
<ul>
<li><p>A csúcspontokat összekötő éleken karakterek szerepelnek.</p></li>
<li><p>A csúcspontokon (belsőkön is, nem csak a leveleken) szerepelnek a tárolt értékek.</p></li>
<li><p>Egy értékhez vezető úton szereplő karakterek összefűzésével áll elő az adott értékhez tartozó kulcs.</p></li>
<li><p>Az üres <code>String</code> a fa gyökérelemét azonosítja.</p></li>
</ul>
<h2 id="a-feladatban-használt-típusok">A feladatban használt típusok</h2>
<ul>
<li><p>A <code>Trie</code> típus első eleme a csúcsban tárolt érték, amely <code>Nothing</code>, ha a csúcs nem tárol értéket.</p></li>
<li><p>A második elem a csúcs alatt szereplő részfákat adja meg. Ez implementációkban általában egy <code>Char</code> kulcsú, <code>Trie</code> értékű map, de a feladatban rendezett párok listájával történik a megvalósítás. A párok első eleme a részfába vezető él címkéje (<code>Char</code>), a második pedig a részfa. A listában a párok az első elem (<code>Char</code>) szerint növekvő sorrendben rendezve szerepelnek.</p></li>
</ul>
<pre class="clean"><code>:: Trie v = T (Maybe v) [(Char, Trie v)]

emptyTrie :: Trie v
emptyTrie = T Nothing []</code></pre>
<h2 id="elem-beszúrása-a-trie-be">Elem beszúrása a <code>Trie</code>-be</h2>
<p>Defináljuk az <code>insertTrie</code> függvényt, amely egy adott <code>String</code> kulcshoz tartozó értéket szúr be a <code>Trie</code>-be. Ha már szerepel a <code>Trie</code>-ben az érték, írjuk azt felül! Az első paraméter a kulcs, a második a beszúrandó érték, a harmadik pedig a módosítandó <code>Trie</code>.</p>
<pre class="clean"><code>insertTrie :: String v (Trie v) -&gt; (Trie v)</code></pre>
<p>Példa:</p>
<pre class="clean"><code>test_insertTrie :: [Bool]
test_insertTrie =
    [ insertTrie ""    0 emptyTrie
	=== T (Just 0) []
    , insertTrie "a"   1 emptyTrie
	=== T Nothing [('a', T (Just 1) [])]

    , insertTrie "b"   2 (insertTrie "a"   1 emptyTrie)
	=== T Nothing [('a', T (Just 1) []), ('b', T (Just 2) [])]
    , insertTrie "a"   1 (insertTrie "b"   2 emptyTrie)
	=== T Nothing [('a', T (Just 1) []), ('b', T (Just 2) [])]

    , insertTrie "ab"  3 (insertTrie "a"   1 emptyTrie)
	=== T Nothing [('a', T (Just 1) [('b', T (Just 3) [])])]
    , insertTrie "abc" 4 emptyTrie
	=== T Nothing [('a', T Nothing [('b', T Nothing [('c', T (Just 4) [])])])]
    ]</code></pre>
<h2 id="a-trie-építése">A <code>Trie</code> építése</h2>
<p>Definiáljuk a <code>buildTrie</code> függvényt, amely kulcs-érték párok listájából előállít egy <code>Trie</code>-t!</p>
<p><strong>A megoldásban használjunk hajtogatást (<code>foldl</code> vagy <code>foldr</code>)!</strong></p>
<pre class="clean"><code>buildTrie :: [(String, v)] -&gt; Trie v</code></pre>
<p>Példa:</p>
<pre class="clean"><code>test_buildTrie :: [Bool]
test_buildTrie =
    [ buildTrie [] === emptyTrieInt
    , buildTrie [("", 0)] === T (Just 0) []
    , buildTrie [("", 0), ("ab", 1), ("abc", 2), ("abd", 3)] ===
	T (Just 0) [('a', T Nothing [('b', T (Just 1) [('c', T (Just 2) []), ('d', T (Just 3) [])])])]
    ]
    where
	emptyTrieInt :: Trie Int
	emptyTrieInt = emptyTrie</code></pre>
<h2 id="egy-elem-keresése-a-trie-ben">Egy elem keresése a <code>Trie</code>-ben</h2>
<p>Definiáljuk a <code>lookupTrie</code> függvényt, amely megkeresi egy <code>Trie</code>-ben a megadott kulcshoz (<code>String</code>) tartozó értéket! Ha a kulcs nem szerepel a <code>Trie</code>-ben, az eredmény legyen <code>Nothing</code>, különben legyen a megtalált érték a <code>Just</code> konstruktorba csomagolva.</p>
<pre class="clean"><code>lookupTrie :: String (Trie v) -&gt; Maybe v</code></pre>
<p>Példa:</p>
<pre class="clean"><code>test_lookupTrie =
    [ lookupTrie "hello" emptyTrieBool == Nothing
    , lookupTrie ""    t == Just 0
    , lookupTrie "a"   t == Nothing
    , lookupTrie "ab"  t == Just 1
    , lookupTrie "abc" t == Just 2
    , lookupTrie "abd" t == Just 3
    , lookupTrie "abe" t == Nothing
    ]
    where
	emptyTrieBool :: Trie Bool
	emptyTrieBool = emptyTrie
	t :: Trie Int
	t = T (Just 0) [('a', T Nothing [('b', T (Just 1) [('c', T (Just 2) []), ('d', T (Just 3) [])])])]</code></pre>
<h2 id="a-functor-típusosztály">A <code>Functor</code> típusosztály</h2>
<p>A <code>Functor</code> típusosztály olyan tároló jellegű típusokat reprezentál, amelyekben tárolt minden értékre lehet alkalmazni egy függvényt.</p>
<pre class="clean"><code>class Functor f where
    fmap :: (a -&gt; b) (f a) -&gt; (f b)</code></pre>
<p>Példák <code>Functor</code> példányokra:</p>
<pre class="clean"><code>instance Functor Maybe where
    fmap f (Just a) = Just (f a)
    fmap _ Nothing  = Nothing

instance Functor [] where
    fmap f ls = map f ls</code></pre>
<p>Példányosítsuk a <code>Functor</code> típusosztályt a <code>Trie</code>-re! Definiáljuk a típusosztály <code>fmap</code> függvényét úgy, hogy a <code>Trie</code>-ben tárolt minden értékre alkalmazza a megadott függvényt!</p>
<p>Tesztek:</p>
<pre class="clean"><code>test_fmap :: [Bool]
test_fmap =
    [ fmap ((+) 1) emptyTrie === emptyTrie
    , fmap ((+) 1) (T (Just 0) []) === (T (Just 1) [])
    , fmap ((+) 1) (T (Just 0) [('a', T Nothing [('b', T (Just 1) [('c', T (Just 2) []), ('d', T (Just 3) [])])])])
            === T (Just 1) [('a', T Nothing [('b', T (Just 2) [('c', T (Just 3) []), ('d', T (Just 4) [])])])]
    , fmap (\x -&gt; if (x rem 2 == 0) "even" "odd") (T (Just 0) [('a', T Nothing [('b', T (Just 1) [('c', T (Just 2) []), ('d', T (Just 3) [])])])])
            === (T (Just "even") [('a', T Nothing [('b', T (Just "odd") [('c', T (Just "even") []), ('d', T (Just "odd") [])])])])
    ]</code></pre>
<p>Itt vegyük észre, hogy akkor fognak működni rendesen a tesztek, ha a <code>gEq</code> generikus műveletet származtatjuk a <code>Maybe</code> és <code>Trie</code> típusok definícióira!</p>
<h2 id="segítség-a-feltöltéshez">Segítség a feltöltéshez</h2>
<p>Az alábbi állományt érdemes módosítani, így, szövegesen kell feltölteni (az alábbi természetesen hibás működésű program).</p>
<p>FONTOS: <em>csak olyan megoldást töltsünk fel, amely <strong>lefordul</strong>!</em></p>
<pre><code>module Trie

import StdEnv, StdLib, GenEq

derive gEq Trie, Maybe

:: Trie v = T (Maybe v) [(Char, Trie v)]

emptyTrie = abort "undefined"

insertTrie :: String v (Trie v) -&gt; (Trie v)
insertTrie = abort "undefined"

buildTrie :: [(String, v)] -&gt; Trie v
buildTrie = abort "undefined"

lookupTrie :: String (Trie v) -&gt; Maybe v
lookupTrie = abort "undefined"

class Functor f where
  fmap :: (a -&gt; b) (f a) -&gt; (f b)

instance Functor Trie where
  fmap = abort "undefined"</code></pre>
<p>A tesztelést az alábbi függvénnyel lehet segíteni:</p>
<pre><code>Start = (and (flatten tests), tests)
  where
    tests =
      [ test_insertTrie
      , test_buildTrie
      , test_lookupTrie
      , test_fmap
      ]</code></pre></div></div>
