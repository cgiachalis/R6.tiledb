# Test 'vfs_size()'

    Code
      .byte_size_format(1023)
    Output
      [1] "1023 B"

---

    Code
      .byte_size_format(1024^1)
    Output
      [1] "1 KiB"

---

    Code
      .byte_size_format(1024^2)
    Output
      [1] "1 MiB"

---

    Code
      .byte_size_format(1024^3)
    Output
      [1] "1 GiB"

---

    Code
      .byte_size_format(1024^4)
    Output
      [1] "1 TiB"

---

    Code
      .byte_size_format(1024^5)
    Output
      [1] "1 PiB"

---

    Code
      .byte_size_format(1024^6)
    Output
      [1] "1 EiB"

---

    Code
      .byte_size_format(1024^7)
    Output
      [1] "1 ZiB"

