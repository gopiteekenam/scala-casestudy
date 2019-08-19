package com.github

import java.time.LocalDateTime

case class Repository (url: String, name: String, id: Int, description: String, readMe: String, languages: Map[String, Long], createdAt: LocalDateTime,
                       updatedAt: LocalDateTime, pushedAt: LocalDateTime, stars: Int, watchers: Int, hasPages: Boolean, forks: Int,
                       defaultBranch: String)

object Repository extends APIResult {
  def apply(result: Map[String, Any], readMe: String, languages: Map[String, Long]): Repository = {
    implicit val apiResponse = result
    val url = getString("url")
    val name = getString("name")
    val id = getInt("id")
    val description = getString("description")
    val createdAt = getDate("created_at")
    val updatedAt = getDate("updated_at")
    val pushedAt = getDate("pushed_at")
    val stars = getInt("stargazers_count")
    val watchers = getInt("watchers_count")
    val hasPages = getBoolean("has_pages")
    val forks = getInt("forks_count")
    val defaultBranch = getString("default_branch")
    Repository(url, name, id, description, readMe, languages, createdAt, updatedAt, pushedAt, stars, watchers, hasPages, forks, defaultBranch)
  }
}