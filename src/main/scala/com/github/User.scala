package com.github

import java.time.LocalDateTime

case class User(login: String, id: Int, name: String, blog: String, location: String, email: String, publicRepos: Int,
                publicGists: Int, followers: Int, following: Int, createdAt: LocalDateTime, updatedAt: LocalDateTime)

object User extends APIResult {
  def apply(result: Map[String, Any]): User = {
    implicit val apiResponse = result
    val login = getString("login")
    val id = getInt("id")
    val name = getString("name")
    val blog = getString("blog")
    val location = getString("location")
    val email = getString("email")
    val publicRepos = getInt("public_repos")
    val publicGists = getInt("public_gists")
    val followers = getInt("followers")
    val following = getInt("following")
    val createdAt = getDate("created_at")
    val updatedAt = getDate("updated_at")
    User(login, id, name, blog, location, email, publicRepos, publicGists, followers, following, createdAt, updatedAt)
  }
}