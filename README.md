# Natools

This library gather all reusable packages written by Natasha Kerensikova
that are too small to fit in a project by themselves.

It contains the following package hierarchy:

  - `Accumulators`: an interface for string accumulator objects and stacks
    of accumulators
      - `String_Accumulator_Linked_Lists`: a basic implementation of an
        accumulator stack using a reference accumulator
  - `Chunked_Strings`: an implementation of unbounded strings backed by
    non-contiguous fixed-size chunks of memory
  - `Cron`: a simple low-precision service of periodic events
  - `Getopt_Long`: command-line argument processing similar to C `getopt_long`
  - `GNAT_HMAC`: instances of `HMAC` using GNAT hash primitives
  - `HMAC`: generic HMAC implementation using a formal hash function
  - `Indefinite_Holders`: simple Ada 2005 implementation of the
     Ada 2012 container
  - `References`: generic simple reference-counter implementation
  - `S-expressions`: library for dealing with [S-expressions][1]
      - `Atom_Buffers`: dynamic buffer for S-expression atoms
      - `Atom_Refs`: common reference-counted atoms
      - `Dynamic_Interpreters`: S-expression interpreter with mutable
        commands and callbacks
      - `Encodings`: translators to and from official S-expression encodings
      - `File_Readers`: objects reading a file to an atom or a S-expression
      - `File_Writers`: file-backed S-expression printer
      - `Generic_Caches`: memory container for S-expressions
      - `Interpeter_Loop`: inner loop of S-expression interpreters,
        typically used in static interpreters
      - `Interpreters`: callback-based S-expressions interpreter
      - `Lockable`: interface for S-expressions descriptors that can be
        temporarily restricted to a given nesting level
      - `Parsers`: S-expression descriptor from a byte stream
      - `Printers`: interface for objects to which S-expressions are written
          - `Pretty`: human-friendly S-expression pretty printer
              - `Config`: serialization and deserialization of pretty printer
                parameters to and from S-expressions
  - `Static_Hash_Maps`: code generator around GNAT `Perfect_Hash_Generators`
    to build a static hash map
      - `S_Expressions`: read S-expression description of static hash map
  - `Storage_Pools`: helper objects with dynamic memory management
  - `String_Slices`: objects hold slices of reference-counted shared strings
      - `Slice_Sets`: sets of aforementionned slices
  - `Tests`: very simple test framework
      - `Text_IO`: test output using standard `Ada.Text_IO`


[1]: http://people.csail.mit.edu/rivest/Sexp.txt
