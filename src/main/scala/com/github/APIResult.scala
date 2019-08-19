package com.github

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

trait APIResult {
  val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")

  def getString(key: String)(implicit apiResponse: Map[String,Any]): String = apiResponse(key).asInstanceOf[String]
  def getInt(key: String)(implicit apiResponse: Map[String,Any]): Int = apiResponse(key).asInstanceOf[Double].toInt
  def getBoolean(key: String)(implicit apiResponse: Map[String,Any]): Boolean = apiResponse(key).asInstanceOf[Boolean]
  def getDate(key: String)(implicit apiResponse: Map[String,Any]): LocalDateTime = LocalDateTime.parse(getString(key), dateFormatter)
}
