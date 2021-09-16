initSidebarItems({"enum":[["SourceChunkInfo","Information about how a [`SourceChunk`] was created."],["SourceLineOverrideKind","Describes the kind of a line override entry (the third argument of `line)."]],"struct":[["SourceChunk","A contiguous chunk of source code, belonging to a [`SourceManager`]. Can be a source file or a macro expansion."],["SourceLineInfo","Result of looking up a line number."],["SourceLoc","A compressed representation of a location in the source code, to be used in ASTs and other long-term storage places.  Only has meaning in relation to a given [`SourceManager`], and can be unpacked into a [`SourceRef`] when necessary.  Consists of a single [`NonZeroU32`] (so that [`Option<SourceLoc>`] is as cheap as a plain [`SourceLoc`], when necessary)."],["SourceManager","A catalogue of all source files that were involved in the creation of an AST / a design / whatever.  New files can be inserted into this given only a shared reference."],["SourceOverrideLineInfo","Information about overriden line number and file name, from `line."],["SourceRange","A half-open range of two [`SourceLoc`]s that may or may not belong to the same [`SourceChunk`]."],["SourceRangeRef","An uncompressed representation of a range of locations in the source code, analogous to [`SourceRef`].  Can be dereferenced to obtain underlying str."],["SourceReader","A helper for reading from a source chunk character by character."],["SourceRef","An uncompressed representation of a location in the source code, to be used for currently-processed code when space is not at premium. The chunk_pos is a direct index in chunk.text and can be used to retrieve the text."],["SourceSimpleLineInfo","Simple decoded location, to be used when exporting to formats that need plain file+line+column triple.  This always gives a file location — when macros are involved, this is the location of the top macro invocation."]]});