# 'TileDBGroup' class tests accessors on empty group

    Code
      group$print()
    Message
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
    Message
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
      > URI Basename: test-group
        * Arrays: "arr1"
        * Groups: "grp1" and "grp2"

---

    Code
      group$print()
    Message
      R6Class: <TileDBGroup>
      > URI Basename: test-group
        * Arrays: ""
        * Groups: "grp1" and "grp2"

---

    Code
      group$print()
    Message
      R6Class: <TileDBGroup>
      > URI Basename: test-group
        * Arrays: ""
        * Groups: "grp2"

---

    Code
      group$print()
    Message
      i R6Class: <TileDBGroup> is empty.

---

    Code
      group$print()
    Message
      i R6Class: <TileDBGroup> object does not exist.

