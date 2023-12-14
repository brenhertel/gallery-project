# gallery-project
 Art Gallery Problem implementation in Haskell for COMP.5300

 Video implementation can be seen here: https://youtu.be/HL1FEZc66XE

 An implementation of the art gallery problem (https://en.wikipedia.org/wiki/Art_gallery_problem) approximation using Delaunay Triangulation (brute force) and 3-coloring.

 To run the program, you can compile the project using either `stack build` and `stack run` or you can use the ghci interpreter by using 
   1. `cd src`
   2. `stack repl`
   3. `:load Main`
   4. `main`

This will plot the polygon, triangulation, and output the coloring and guard locations to the screen. Additionally, there are 3 polygons implemented in the `Main.hs` file. To choose different polygons, edit line 299 of `Main.hs` to either `generatePolygon1`, `generatePolygon2`, or `generatePolygon3`. Note that you will need the packages random (https://hackage.haskell.org/package/random) and matplotlib (https://hackage.haskell.org/package/matplotlib) installed.
