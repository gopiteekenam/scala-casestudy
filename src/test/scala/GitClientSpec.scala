import com.github.{GitClient, Qualifier, SearchQuery}
import org.scalatest.FunSuite

class GitClientSpec extends FunSuite{
  val token: Option[String] = GitClient.getToken
  val client = new GitClient(token)

  test("getUser") {
    val user = client.getUser("gopiteekenam")
    println(user)
    assert(user.id == 50986656)
  }

  test("getRepos") {
    val repos = client.getRepos("gopiteekenam")
    assert(repos.exists(repo => repo.name == "ScalaProject"))
  }

  test("getRepo") {
    val repo = client.getRepo("gopiteekenam", "springboot")
    assert(repo.name == "springboot")
  }
  test("getRepoWithLanguageAndReadMe") {
    val repoWithStuff = client.getRepo("gopiteekenam", "ScalaProject", withLanguages=true, withReadMe=true)
    val repoWithoutStuff = client.getRepo("gopiteekenam", "springboot")
    assert(repoWithStuff.languages.nonEmpty && repoWithStuff.readMe.nonEmpty)
    assert(repoWithoutStuff.languages.isEmpty && repoWithoutStuff.readMe.isEmpty)
  }

  test("searchRepos") {
    val query = SearchQuery("", Map(("user",Qualifier("user","gopiteekenam")),("language",Qualifier("language","Scala"))), Map("sort" -> "stars", "order" -> "asc"))
    val repos = client.searchRepos(query)
    assert(repos.exists(repo => repo.name == "ScalaProject"))
  }
}
