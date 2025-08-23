# 'TileDBArray' class tests on non-existent array

    Code
      arrObj$print()
    Message
      i R6Class: <TileDBArray> object does not exist.

# 'TileDBArray' class works as expected

    Code
      arrObj$print()
    Message
      R6Class: <TileDBArray>
      > URI Basename: test-TileDBArray
        * Dimensions: "Dept" and "Gender"
        * Attributes: "Admit" and "Freq"

# Test metadata print method

    Code
      arrObj$get_metadata()
    Output
      TileDB ARRAY: <R6 Class: TileDBArray>
      Metadata: <key,value> * total 0
      

---

    Code
      arrObj$get_metadata()
    Output
      TileDB ARRAY: <R6 Class: TileDBArray>
      Metadata: <key,value> * total 5
       * a: 'Hi'
       * b: 'good'
       * c: 10
       * d: 'Boo'
       * e: 3
      

