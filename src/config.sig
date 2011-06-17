signature CONFIG = sig
    val bin : string
    val lib : string
    val includ : string
    val sitelisp : string

    val libUr : string
    val libC : string
    val libJs : string

    val ccompiler : string
    val gccArgs : string
    val openssl : string

    val pgheader : string
    val msheader : string
    val sqheader : string
end
