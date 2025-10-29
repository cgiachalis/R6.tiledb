# 'TileDBFragments' class works as expected

    Code
      val <- fragObj$delete_fragment(1)
    Message
      i No fragments found to delete.

# Test ifragments methods

    Code
      ifrag
    Output
      -- FRAGMENT #1 -----------------------------------------------------------------
       > URI: __fragments/__1000_1000_28109_22
       > Type: sparse
       > Non-empty domain: 
         * id: [1, 1] (INT32)
       > Size: 3.14 KiB
       > Cell num: 1
       > Timestamp range: [1970-01-01 00:00:01 UTC, 1970-01-01 00:00:01 UTC]
       > Format version: 22
       > Has consolidated metadata: FALSE
      

---

    Code
      ifrag_list
    Output
      -- FRAGMENT #1 -----------------------------------------------------------------
       > URI: __fragments/__1000_1000_28109_22
       > Type: sparse
       > Non-empty domain: 
         * id: [1, 1] (INT32)
       > Size: 3.14 KiB
       > Cell num: 1
       > Timestamp range: [1970-01-01 00:00:01 UTC, 1970-01-01 00:00:01 UTC]
       > Format version: 22
       > Has consolidated metadata: FALSE
      
      -- FRAGMENT #2 -----------------------------------------------------------------
       > URI: __fragments/__2000_2000_fd109_22
       > Type: sparse
       > Non-empty domain: 
         * id: [1, 1] (INT32)
       > Size: 3.13 KiB
       > Cell num: 1
       > Timestamp range: [1970-01-01 00:00:02 UTC, 1970-01-01 00:00:02 UTC]
       > Format version: 22
       > Has consolidated metadata: FALSE
      

