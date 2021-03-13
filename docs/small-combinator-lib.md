# Small Combinator Library

After finishing the first section of *Domain Specific Language* we are
presented with implementations of several small combinator
functions. I want to summarize my particular opinions on the subject
and specific decisions used during implementation.


- Definitely interesting and on daily job is hard to get ourselves thinking in this style
- I don't like to rely on **positional** arguments
  - Using open hashmaps would be a lot better in my opinion
  - The flexibility that open hashmaps provide is very aligned with the proposed rationale
- `assert` in every function is bad
  - Not the assert itself, but often the error messages are very opaque and hard to debug
  - `s/valid?` using `clojure.specs` seems good alternative, but something opaque too
  - I prefer to have this validations in the borders of the system,
    but as we are building a library that's fine.
- Often the order of arguments is not the desired everywhere, I like
  the idea of using `permute-arguments` to make it explicit that we
  have this problem. Often I implemented a new function with desired
  order and called my function from inside, but this approach is not
  clear that we had a permutation problem.
