object PriceBasket {
  def getPrice (ShoppingList : Array[String], DiscountWeek : Boolean = false): Unit = {
    // Initialise variables
    var subtotal : Double = 0
    var total : Double = 0
    var appleDiscount : Double = 0
    var soupDiscount : Double = 0
    var nsoup : Double = 0
    var nbread : Int = 0
    var napples : Int = 0
    var nmilk : Int = 0
    var promoArray  = Array.empty[String]
    var havent = Array.empty[String]
    
    // Iterate through each item in ShoppingList Array
    for (item <- ShoppingList){
      // Normalise items to all be lower case
      var item2: String = item.toLowerCase()  
      
      // Account for items not in stock (may be able to compare to Array/seq of stocked items rather than series of ORs)
      if (!(item2 == "soup" || item2 == "bread" || item2 == "milk" || item2 == "apples")){
        havent :+= item
      }
      
      // Count number of each item in ShoppingList
      item2 match {
        case "soup" => nsoup += 1
        case "bread" => nbread += 1
        case "milk" => nmilk += 1
        case "apples" => napples += 1
        case _ => //do nothing
      }
    }
    
    // Calculate subtotal (before possible discounts)
    subtotal = (nsoup*0.65) + (nbread*0.8) + (nmilk*1.3) + (napples*1.0)
    
    // Account for any possible discounts to total price
    if (DiscountWeek == true && napples >= 1) {
      appleDiscount += (0.1*napples)
      promoArray :+= raw"Apples 10% off: " + f"-£$appleDiscount%1.2f"
    }
    var dealOpps : Double = (nsoup/2).floor
    if (dealOpps >= 1 && nbread >= 1) {
      soupDiscount += (0.4*dealOpps.min(nbread))
      promoArray :+= raw"2 Soups 50% off Bread: " + f"-£$soupDiscount%1.2f"
    }
    total = subtotal - appleDiscount - soupDiscount
    
    // Print out relevant information (out of stock items, subtotal, total, discounts)
    if (!(havent.isEmpty)){
      var haventStr : String = havent.mkString(", ")
      println(f"Sorry, it seems we don't have the following item(s) in stock: $haventStr")
    }
    println(f"Subtotal: £$subtotal%1.2f\nTotal price: £$total%1.2f");
    if (!(promoArray.isEmpty)){
      var promoStr : String = promoArray.mkString(", ")
      println(f"Promotions: $promoStr")
    } else {
      println("Promotions: (No offers available)")
    }
  }

  def main(args:Array[String]): Unit = {
    getPrice(args, true)
  }
}
