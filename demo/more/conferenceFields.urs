val title : Meta.meta (string, string)
val abstract : Meta.meta (string, string)
val commentsForAuthors : Meta.meta (string, string)

con dropdown :: (Type * Type)
val dropdown : string -> list char -> Meta.meta dropdown
val dropdown_show : show dropdown.1
