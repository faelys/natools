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
  - `Constant_Indefinite_Ordered_Maps`: task-safe ordered maps with immutable
    mapping
  - `Cron`: a simple low-precision service of periodic events
  - `File_Streams`: wrapper around `Stream_IO` files implementing stream
    interface
  - `Getopt_Long`: command-line argument processing similar to C `getopt_long`
  - `GNAT_HMAC`: instances of `HMAC` using GNAT hash primitives
  - `HMAC`: generic HMAC implementation using a formal hash function
  - `Indefinite_Holders`: simple Ada 2005 implementation of the
     Ada 2012 container
  - `References`: generic simple reference-counter implementation
      - `Pools`: task-safe pool of referencesjj
  - `S-expressions`: library for dealing with [S-expressions][1]
      - `Atom_Buffers`: dynamic buffer for S-expression atoms
      - `Atom_Ref_Constructors`: helper constructors for atom references
      - `Atom_Refs`: common reference-counted atoms
      - `Conditionals`: S-expression boolean expressions about some object
          - `Generic_Evaluate`: Generic boolean expression evaluation framework
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
      - `Replayable`: interface for S-expression descriptors whose state can
        be stored and replayed
      - `Special_Descriptors`: always-empty and always-in-error descriptors
      - `Templates`: S-expression template renderers
          - `Dates`: rendering of `Ada.Calendar.Time` values
          - `Generic_Discrete_Render`: rendering of enumeration values
          - `Generic_Integers`: rendering of integer values
          - `Integers`: instance of `Generic_Integers` for standard integers
  - `Static_Hash_Maps`: code generator around GNAT `Perfect_Hash_Generators`
    to build a static hash map
      - `S_Expressions`: read S-expression description of static hash map
  - `Storage_Pools`: helper objects with dynamic memory management
  - `String_Slices`: objects hold slices of reference-counted shared strings
      - `Slice_Sets`: sets of aforementionned slices
  - `Tests`: very simple test framework
      - `Text_IO`: test output using standard `Ada.Text_IO`
  - `Time_IO`: conversions between time values and strings
      - `Human`: human-readable fuzzy formats
      - `RFC_3339`: time format described by
        [RFC-3339](http://tools.ietf.org/html/rfc3339)
  - `Time_Keys`: short printable string serialization of time that is
    consistent with lexicographical order
  - `Time_Statistics`: accumulator for (run)time statistics
      - `Coarse_Timers`: instance of `Generic_Timers` with
        standard calendar time
      - `Fine_Timers`: instance of `Generic_Timers` using realtime annex
      - `Generic_Timers`: timer objects to provide data to accumulators


[1]: http://people.csail.mit.edu/rivest/Sexp.txt
