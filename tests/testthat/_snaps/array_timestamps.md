# Test 'array_timestamps()' works as expected

    Code
      array_timestamps(arrobj)
    Output
      Array Timestamps * Mode (read) * TZ (Europe/London)
       Temporal Range
        * start: none
        * end  : 2020-08-20 21:00:00
       Open Range
        * start: 1970-01-01 01:00:00
        * end  : 2020-08-20 21:00:00

---

    Code
      array_timestamps(arrobj)
    Output
      Array Timestamps * Mode (closed) * TZ (Europe/London)
       Temporal Range
        * start: none
        * end  : none
       Open Range
        * start: 1970-01-01 01:00:00
        * end  : 2020-08-20 21:00:00

