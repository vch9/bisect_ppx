Reporter still works even in the presence of line number directives.

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test2)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ cat test.ml - >> test2.ml <<'EOF'
  > 
  > # 1 "other_file.ml"
  > 
  > let h () =
  >   ()
  > 
  > let () =
  >   h ()
  > EOF
  $ cat test2.ml
  let f () =
    ()
  
  let g () =
    ()
  
  let () =
    f ()
  
  (* Reproduces a HTML display bug that existed in development between 2.6.3 and
     2.7.0, starting with 1b8d7ec5985aa12a85e797e3d53fc72713e80c35. *)
  let a () =
    if true then
      true
    else
      false
  
  # 1 "other_file.ml"
  
  let h () =
    ()
  
  let () =
    h ()
  $ dune exec ./test2.exe --instrument-with bisect_ppx
  $ bisect-ppx-report html --verbose
  Info: found *.coverage files in './'
  Info: Writing index file...
  $ cat _coverage/test2.ml.html
  <!DOCTYPE html>
  <html lang="en">
    <head>
      <title>Coverage report</title>
      <link rel="stylesheet" href="coverage.css"/>
      <script src="highlight.pack.js"></script>
      <script>hljs.initHighlightingOnLoad();</script>
      <meta charset="utf-8"/>
    </head>
    <body>
      <div id="header">
        <h1>
          <a href="index.html">
            <span class="dirname"></span>test2.ml
          </a>
        </h1>
        <h2>50.00%</h2>
      </div>
      <div id="navbar">
        <span class="unvisited" style="top:20.83%"></span>
        <span class="unvisited" style="bottom:45.83%"></span>
        <span class="unvisited" style="bottom:41.67%"></span>
        <span class="unvisited" style="bottom:33.33%"></span>
      </div>
      <div id="report">
        <div id="lines-layer">
          <pre>
  <a id="L1"></a><span > </span>
  <a id="L2"></a><span class="visited"> </span>
  <a id="L3"></a><span > </span>
  <a id="L4"></a><span > </span>
  <a id="L5"></a><span class="unvisited"> </span>
  <a id="L6"></a><span > </span>
  <a id="L7"></a><span > </span>
  <a id="L8"></a><span class="visited"> </span>
  <a id="L9"></a><span > </span>
  <a id="L10"></a><span > </span>
  <a id="L11"></a><span > </span>
  <a id="L12"></a><span > </span>
  <a id="L13"></a><span class="unvisited"> </span>
  <a id="L14"></a><span class="unvisited"> </span>
  <a id="L15"></a><span > </span>
  <a id="L16"></a><span class="unvisited"> </span>
  <a id="L17"></a><span > </span>
  <a id="L18"></a><span > </span>
  <a id="L19"></a><span > </span>
  <a id="L20"></a><span > </span>
  <a id="L21"></a><span class="visited"> </span>
  <a id="L22"></a><span > </span>
  <a id="L23"></a><span > </span>
  <a id="L24"></a><span class="visited"> </span>
  </pre>
        </div>
        <div id="text-layer">
          <pre id="line-numbers">
  <a href="#L1"> 1</a>
  <a href="#L2"> 2</a>
  <a href="#L3"> 3</a>
  <a href="#L4"> 4</a>
  <a href="#L5"> 5</a>
  <a href="#L6"> 6</a>
  <a href="#L7"> 7</a>
  <a href="#L8"> 8</a>
  <a href="#L9"> 9</a>
  <a href="#L10">10</a>
  <a href="#L11">11</a>
  <a href="#L12">12</a>
  <a href="#L13">13</a>
  <a href="#L14">14</a>
  <a href="#L15">15</a>
  <a href="#L16">16</a>
  <a href="#L17">17</a>
  <a href="#L18">18</a>
  <a href="#L19">19</a>
  <a href="#L20">20</a>
  <a href="#L21">21</a>
  <a href="#L22">22</a>
  <a href="#L23">23</a>
  <a href="#L24">24</a>
  </pre>
  <pre><code class="ocaml">let f () =
    <span data-count="1">(</span>)
  
  let g () =
    <span data-count="0">(</span>)
  
  let () =
    <span data-count="1">f</span> ()
  
  (* Reproduces a HTML display bug that existed in development between 2.6.3 and
     2.7.0, starting with 1b8d7ec5985aa12a85e797e3d53fc72713e80c35. *)
  let a () =
    <span data-count="0">i</span>f true then
      <span data-count="0">t</span>rue
    else
      <span data-count="0">f</span>alse
  
  # 1 "other_file.ml"
  
  let h () =
    <span data-count="1">(</span>)
  
  let () =
    <span data-count="1">h</span> ()
  </code></pre>
        </div>
      </div>
      <script src="coverage.js"></script>
    </body>
  </html>
