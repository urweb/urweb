signature CONFIG = sig
    val bin : string
    val lib : string
    val includ : string
    val sitelisp : string

    val libUr : string
    val libC : string
    val libJs : string

    val gccArgs : string
    val libMhash : string
end
