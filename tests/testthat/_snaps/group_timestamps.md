# Test 'group_timestamps()' works as expected

    Code
      group_timestamps(group, from = "ctx", tz = "UTC")
    Output
      Group Timestamps (ctx) * Mode (closed) * TZ (UTC)
       * start: 1990-01-01 00:00:00
       * end  : 2020-08-20 20:00:00

---

    Code
      group_timestamps(group, from = "cfg", tz = "UTC")
    Output
      Group Timestamps (group config) * Mode (closed) * TZ (UTC)
       * start: 1990-01-01 00:00:00
       * end  : 2020-08-20 20:00:00

---

    Code
      group_timestamps(group$object, tz = "UTC")
    Output
      Group Timestamps (group config) * Mode (read) * TZ (UTC)
       * start: 1990-01-01 00:00:00
       * end  : 2020-08-20 20:00:00

---

    Code
      group_timestamps(group$ctx, tz = "Europe/London")
    Output
      Group Timestamps (ctx) * Mode (N/A) * TZ (Europe/London)
       * start: 1990-01-01 00:00:00
       * end  : 2020-08-20 21:00:00

---

    Code
      group_timestamps(tiledb::config(group$ctx), tz = "Europe/London")
    Output
      Group Timestamps (config) * Mode (N/A) * TZ (Europe/London)
       * start: 1990-01-01 00:00:00
       * end  : 2020-08-20 21:00:00

