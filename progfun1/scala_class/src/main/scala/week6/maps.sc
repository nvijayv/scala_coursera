object maps {
    val romanNumerals = Map('I' -> 1, 'V' -> 5, 'X' -> 10)
    val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

    capitalOfCountry("US")
    capitalOfCountry get "US"
    capitalOfCountry get "Andorra"

    def showCapital(country: String) = capitalOfCountry.get(country) match {
        case Some(capital) => capital
        case None => "missing data"
    }

    showCapital("US")
    showCapital("Andorra")

    val fruits: List[String] = List("apple", "pear", "orange", "pineapple")
    fruits sortWith (_.length < _.length)
    fruits.sorted

    fruits groupBy (_.head)
}