# 'TileDBArray' class works as expected

    Code
      arrObj$print()
    Message
      i R6Class: <TileDBArray> object does not exist.

---

    Code
      arrObj$print()
    Message
      R6Class: <TileDBArray>
      > URI Basename: test-TileDBArray
        * Dimensions: "Dept" and "Gender"
        * Attributes: "Admit" and "Freq"

---

    Code
      arrObj$get_metadata()
    Output
      TileDB ARRAY: <R6 Class: TileDBArray>
      Metadata: <key,value>
      

---

    Code
      arrObj$get_metadata()
    Output
      TileDB ARRAY: <R6 Class: TileDBArray>
      Metadata: <key,value>
       * a: 'Hi'
       * b: 'good'
       * c: 10
       * d: 'Boo'
       * e: 3
      

