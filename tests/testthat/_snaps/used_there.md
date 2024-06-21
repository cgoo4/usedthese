# Resource unavailable

    Code
      used_there("https://quantumjitter.com/project2", 1)
    Condition
      Error in `used_there()`:
      ! https://quantumjitter.com/project2 is currently unavailable.
      i Verify the URL or try again later.
      Caused by error in `open.connection()`:
      ! HTTP error 404.

---

    Code
      used_there("https://quantumjitter.com/project", 1:3)
    Condition
      Error in `used_there()`:
      ! `num_links` must be a scalar
      x You've supplied a <integer> object of length 3.

