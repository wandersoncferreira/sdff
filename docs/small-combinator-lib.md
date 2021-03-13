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
  - `s/valid?` using `clojure.specs` seems good alternative, but sometimes opaque too
  - I prefer to have these validations at the borders of the system,
    but as we are building a library that's fine.
- I like the idea of using `permute-arguments` to make it explicit
  that we have this problem. Often I implemented a new function with
  desired order and called my function from inside, but this approach
  is not clear that we had a permutation problem.
- A case for *higher-order* function: they are nice and useful, but also
  difficult to debug (which goes against the rationale for flexible
  system here) and require more overhead for readers of your program
  to understand them.
  
  
### Open questions

- Yet very helpful and clever, but we are still talking about a very
  'low-level' portion of our system. I would agree that we need
  building blocks that are itself flexible, but I am curious to see
  how this will impact the changes we need to perform due to
  uncertainties in business decisions.

	- I mean, the components that are usually discussed inside a
      company are not so low-level, we have huge features whose
      behavior needs to be altered in unexpected ways.
 
    - For now, looks to me that to be able to think about these
      combinators can help us decide in each level of our current
      abstractions in our systems do we need to change by providing
      additional code that performs new behavior and 'redirect' the
      flow of execution to these functions using these dispatch
      mechanisms.
