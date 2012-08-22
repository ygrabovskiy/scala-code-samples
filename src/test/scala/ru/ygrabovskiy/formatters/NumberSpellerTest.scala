package ru.ygrabovskiy.formatters

import org.scalatest.FlatSpec
import org.scalatest.matchers.MustMatchers
import ru.ygrabovskiy.formatters.NumberSpeller.OrderName

class NumberSpellerTest  extends  FlatSpec with MustMatchers {   
  "NumberSpeller"  should "spell numbers correctly" in {
    val ns = NumberSpeller
    ns.spellMoney(0.00) must be ("Ноль рублей 00 копеек")
    ns.spellNumber(0, OrderName("кружка", "кружки", "кружек", false)) must be ("ноль кружек")
    ns.spellMoney(0.45) must be ("Ноль рублей 45 копеек")
    ns.spellMoney(999.12) must be ("Девятьсот девяносто девять рублей 12 копеек")
    ns.spellMoney(1111) must be ("Одна тысяча сто одиннадцать рублей 00 копеек")
    ns.spellMoney(11111) must be ("Одиннадцать тысяч сто одиннадцать рублей 00 копеек")
    ns.spellMoney(111111) must be ("Сто одиннадцать тысяч сто одиннадцать рублей 00 копеек")
    ns.spellMoney(1111111) must be ("Один миллион сто одиннадцать тысяч сто одиннадцать рублей 00 копеек")
    ns.spellMoney(1234567890) must be ("Один миллиард двести тридцать четыре миллиона пятьсот шестьдесят семь тысяч восемьсот девяносто рублей 00 копеек")
    ns.spellMoney(1000000000.999) must be ("Один миллиард рублей 99 копеек")
    ns.spellNumber(134, OrderName("кружка", "кружки", "кружек", false)) must be ("сто тридцать четыре кружки")  
  }
}