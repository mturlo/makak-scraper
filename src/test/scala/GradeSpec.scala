import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GradeSpec
  extends AnyWordSpec
    with Matchers {

  import Grade._

  "Grade" should {

    "increment" in {

      "4A".grade.increment mustBe "4A+".grade
      "4A+".grade.increment mustBe "4A+/B".grade
      "4A+/B".grade.increment mustBe "4B".grade
      "4B".grade.increment mustBe "4B+".grade
      "4B+".grade.increment mustBe "4B+/C".grade
      "4B+/C".grade.increment mustBe "4C".grade
      "4C".grade.increment mustBe "4C+".grade
      "4C+".grade.increment mustBe "4C+/5A".grade
      "4C+/5A".grade.increment mustBe "5A".grade

    }

  }

}
