package com.github

case class Qualifier(left: String, right: String, negate: Boolean = false) {
  override def toString: String = (if (negate) "-" else "") + left + ":\"" + right + "\""
}

/** Consult https://developer.github.com/v3/search/ and https://help.github.com/articles/search-syntax/ for details on
  * parameters and qualifiers.
  */
case class SearchQuery(query: String, qualifiers: Map[String,Qualifier] = Map(), parameters: Map[String,String] = Map()) {

  /**
    * Returns the parameters for the query.
    * @return a sequence of key-value pairs
    */
  def toParams: Seq[(String, String)] = {
    ("q", query + getQualifierString) +: parameters.toSeq
  }
  /**
    * Concatenates all the qualifiers of the query.
    * The qualifiers are the options included in the "q=_" parameter of the query, and are different from the other
    * parameters (e.g. sort, order)
    * @return the string of concatenated qualifiers
    */
  def getQualifierString: String = {
    qualifiers.foldLeft("")(
      (curr: String, pair: (String, Qualifier)) => {
        val (_, qual) = pair
        curr + " " + qual.toString
      }
    )
  }
}
