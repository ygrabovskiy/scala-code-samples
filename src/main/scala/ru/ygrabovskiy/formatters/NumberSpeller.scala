package ru.ygrabovskiy.formatters

import java.text.DecimalFormat

trait StringHelper {

  implicit def toDelimitable(obj1: AnyRef) = new {
    def +?+(obj2: AnyRef) = delimitOptionally(obj2, " ")

    def +:?+(obj2: AnyRef) = delimitOptionally(obj2, ": ")

    def dot(obj2: AnyRef) = delimitOptionally(obj2, ".")

    def delimitOptionally(obj2: AnyRef, delimiter: String) = Seq(obj1, obj2).flatMap(Option(_)).map(_.toString).filter(_.nonEmpty).mkString(delimiter)
  }

  implicit def toNullOrEmpty(str: String) = new {
    def isNullOrEmpty = Option(str).filter(_.nonEmpty).isEmpty
  }
}

object NumberSpeller extends StringHelper with CentsExtractor {

  val ordersInTriple = 3
  val hundreds = Seq("", "сто", "двести", "триста", "четыреста", "пятьсот", "шестьсот", "семьсот", "восемьсот", "девятьсот")
  val firstTenMasculine = Seq("", "один", "два", "три", "четыре", "пять", "шесть", "семь", "восемь", "девять")
  val firstTenFeminine = Seq("", "одна", "две", "три", "четыре", "пять", "шесть", "семь", "восемь", "девять")
  val secondTen = Seq("десять", "одиннадцать", "двенадцать", "тринадцать", "четырнадцать", "пятнадцать", "шестнадцать", "семнадцать", "восемнадцать", "девятнадцать")
  val tens = Seq("двадцать", "тридцать", "сорок", "пятьдесят", "шестьдесят", "семьдесят", "восемьдесят", "девяносто")

  case class OrderName(spellingSingle: String, spellingFrom2to4: String, spellingFrom5: String, isMasculine: Boolean)

  def thousandOrderNames(zeroOrder: OrderName) = Seq(
    zeroOrder,
    OrderName("тысяча", "тысячи", "тысяч", false),
    OrderName("миллион", "миллиона", "миллионов", true),
    OrderName("миллиард", "миллиарда", "миллиардов", true)
  )

  def spellMoney(number: BigDecimal) = spellIntegerPart(number) +?+ spellCents(number)

  private def spellIntegerPart(number: BigDecimal) = capitalize(spellNumber(number.toBigInt()))

  private def spellCents(number: BigDecimal) = extractCents(number) + " копеек"

  def spellNumber(number: scala.BigInt, zeroOrder: OrderName = OrderName("рубль", "рубля", "рублей", true)) = {
    if (number == 0) "ноль " + zeroOrder.spellingFrom5
    else
      splitToTriples(number, zeroOrder).foldLeft("") {
        case (spelled, triple) => {
          triple.spell +?+ spelled
        }
      }
  }

  private def splitToTriples(number: BigInt, zeroOrder: OrderName) = {
    def recursive(number: BigInt, order: Int): Seq[TripleNumberComponent] =
      if (number > 0) {
        val nextOrder = order + ordersInTriple
        val (highPart, lowPart) = splitByOrderOfTen(number, nextOrder)
        new TripleNumberComponent(lowPart, order, zeroOrder) +: recursive(highPart, nextOrder)
      }
      else
        Seq()

    recursive(number, 0)
  }

  private def splitByOrderOfTen(number: BigInt, order: Int) = {
    val lowPart = number % BigInt(10).pow(order)
    val highPart = number - lowPart
    (highPart, lowPart)
  }

  private def capitalize(s: String) = s.headOption.map(_.toUpper + s.tail).getOrElse("")

  class TripleNumberComponent(triple: BigInt, order: Int, zeroOrder: OrderName) {
    val normalized = prefixByZerosToFullTriple(triple.toString())
    val hundred = normalized.head.toString.toInt
    val underHundred = normalized.slice(1, 3).toInt
    val orderName = thousandOrderNames(zeroOrder)(order / ordersInTriple)

    def prefixByZerosToFullTriple(tripleStr: String) = {
      val len = tripleStr.length
      val paddingLen = {
        val r = len % ordersInTriple
        if (r != 0) (ordersInTriple - r) else 0
      }
      "0" * paddingLen + tripleStr
    }

    def spell = {
      spellHundred +?+ spellUnderHundred +?+ spellOrderIfNeed
    }

    private def spellHundred = {
      hundreds(hundred)
    }

    private def spellUnderHundred = {
      underHundred match {
        case inFirstTen if inFirstTen < 10 => spellFirstTenByGender(inFirstTen)
        case inSecondTen if inSecondTen < 20 => spellSecondTen(inSecondTen)
        case overTeen if overTeen >= 20 => spellOverTeen(overTeen)
      }
    }

    private def spellOrderIfNeed =
      if (triple != 0 || order == 0) {
        declineOrderName
      }
      else
        ""

    private def spellTens(ts: Int) = tens(ts - 2)

    private def spellFirstTenByGender(firstTen: Int) =
      (
        if (orderName.isMasculine)
          firstTenMasculine
        else
          firstTenFeminine
        )(firstTen)

    private def spellSecondTen(st: Int) = secondTen(st - 10)

    private def spellOverTeen(overTeen: Int) = {
      val tens = overTeen / 10
      val underTen = overTeen % 10
      spellTens(tens) +?+ spellFirstTenByGender(underTen)
    }

    def declineOrderName =
      (underHundred / 10, underHundred % 10) match {
        case (1, _) => orderName.spellingFrom5
        case (_, 1) => orderName.spellingSingle
        case (_, n) if n >= 5 || n == 0 => orderName.spellingFrom5
        case (_, n) if n >= 2 && n < 5 => orderName.spellingFrom2to4
      }
  }

}

trait CentsExtractor {
  protected def extractCents(number: BigDecimal) = new DecimalFormat("00").format(((number % 1) * 100).toInt)
}

