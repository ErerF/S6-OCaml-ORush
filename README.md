# S6-OCaml-ORush
Simplified version of Klotski game written in OCaml

-------------Introduction-------------
port：a 6*6 grid
boat: described by 5 characters -- ILOXY
      I: id (one char chosen from 'A' to 'Z')
      L: length (by number of cases: 2 or 3)
      O: orientation (V: vertical, H: horizontal)
      X, Y: coordinates of the highest and leftmost cell in the grid
      
      eg. A2H12
          B3V00
          C2V42
movement: '>': move forward
          '<': move back
