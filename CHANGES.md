## Release v0.17.0

- [ppx_streamable] now supports for variant type constructors with inline record payloads.
- [ppx_streamable] now builds on OCaml 5.1
- added [Expert] [Direct_stream_writer] functions to State and Plain RPCs.
- Removed redundang [bin_size_t] calls when streaming types with repetition (e.g. lists,
  maps)
