# 'TileDBGroup' class tests on non-existent group

    Code
      group$print()
    Message
      i R6Class: <TileDBGroup> object does not exist.

# 'TileDBGroup' class tests accessors on empty group

    Code
      group$print()
    Output
      i R6Class: <TileDBGroup> is empty.

---

    Code
      group$dump("Test TileDB")
    Output
      
      -- Test TileDB -----------------------------------------------------------------
      
      -> test-group GROUP

---

    Code
      group$dump(NULL)
    Output
      -> test-group GROUP

# 'TileDBGroup' class tests add/remove members

    Code
      group$print()
    Output
      i R6Class: <TileDBGroup> is empty.

# 'TileDBGroup' class tests print method

    Code
      group$dump("Test Dump TileDB with members")
    Output
      
      -- Test Dump TileDB with members -----------------------------------------------
      
      -> test-group GROUP
      |-- arr1 ARRAY
      |-- grp1 GROUP
      |-- grp2 GROUP

---

    Code
      group$print()
    Message
      R6Class: <TileDBGroup>
    Output
      > URI Basename: test-group
        * Arrays: "arr1"
        * Groups: "grp1" and "grp2"

---

    Code
      group$print()
    Message
      R6Class: <TileDBGroup>
    Output
      > URI Basename: test-group
        * Groups: "grp1" and "grp2"

---

    Code
      group$print()
    Message
      R6Class: <TileDBGroup>
    Output
      > URI Basename: test-group
        * Groups: "grp2"

---

    Code
      group$print()
    Output
      i R6Class: <TileDBGroup> is empty.

---

    Code
      group$print()
    Message
      i R6Class: <TileDBGroup> object does not exist.

# 'TileDBGroup' class tests metadata print method

    Code
      group$get_metadata()
    Output
      TileDB GROUP: <R6 Class: TileDBGroup>
      Metadata: <key,value> * total 0
      

# 'TileDBGroup' class tests metadata

    Code
      group$get_metadata()
    Output
      TileDB GROUP: <R6 Class: TileDBGroup>
      Metadata: <key,value> * total 2
       * a: 'a'
       * b: 100
      

---

    Code
      group$get_metadata()
    Output
      TileDB GROUP: <R6 Class: TileDBGroup>
      Metadata: <key,value> * total 22
       * a: 'a'
       * b: 100
       * av: 1
       * bv: 2
       * cv: 3
       * dv: 4
       * ev: 5
       * fv: 6
       * gv: 7
       * hv: 8
       * iv: 9
       * jv: 10
       * kv: 11
       * lv: 12
       * mv: 13
       * nv: 14
       * ov: 15
       * pv: 16
       * qv: 17
       * rv: 18
       ... and 2 more metadata

