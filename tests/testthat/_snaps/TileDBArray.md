# TileDBArray print() snapshot for non-existent arrays

    Code
      arr$print()
    Message
      i R6Class: <TileDBArray> object does not exist.

# TileDBArray print() snapshot for non-empty array

    Code
      arr$print()
    Message
      R6Class: <TileDBArray>
    Output
      > URI Basename: test-nonempty -array
        * Dimensions: "Dept" and "Gender"
        * Attributes: "Admit" and "Freq"

# TileDBArray metadata print method

    Code
      arr$get_metadata()
    Output
      TileDB ARRAY: <R6 Class: TileDBArray>
      Metadata: <key,value> * total 0
      

---

    Code
      arr$get_metadata()
    Output
      TileDB ARRAY: <R6 Class: TileDBArray>
      Metadata: <key,value> * total 6
       * a: 'Hi'
       * b: 'good'
       * c: 10
       * d: 'Boo'
       * e: 3
       * f: 'abcdefghijklmnopqrstuabcdefghijklmnopqrstuabcdefghijklmnopqr...'
      

