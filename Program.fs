open System


    type Suit = |Spades
                |Clubs
                |Diamonds
                |Hearts

    type Rank = |Value of int
                |Jack
                |Queen
                |King
                |Ace

                static member GetAllRanks() =
                 [for i in 2 .. 10 do yield Value i
                  yield Jack
                  yield Queen
                  yield King
                  yield Ace ]

    type Card = {Suit: Suit; Rank: Rank}

    type origDeck = {
                    Card: Card 
                    Deck: List<Card>}

    type playerHand = {Cards:List<Card>}
    

    let pushToDeck (card, deck) = card :: deck

    //Create a 52-card deck and return it

    let createFullDeck() =
        let mutable fullDeck:List<Card> = []
        for suit in [Spades;Clubs;Diamonds;Hearts] do
            for rank in Rank.GetAllRanks() do
                fullDeck <- pushToDeck ({Rank = rank; Suit = suit}, fullDeck)
        fullDeck

    // Returns the i:th card of the list

    let getCard (deck:List<Card>, i) = deck.Item(i)
    
    //Fold a card and return the remaining deck without the folded card

    let removeCardFromDeck (deck:List<Card>) i =
        let mutable remainingDeck:List<Card> = []
        for j in 0..i-1 do
            remainingDeck <- deck.Item(j) :: remainingDeck
        for j in i+1..deck.Length-1 do
            remainingDeck <- deck.Item(j) :: remainingDeck
        remainingDeck

    let dealNewCard(deck:List<Card>)=
        let rnd = Random()
        let x = rnd.Next(0, deck.Length)
        let card = getCard(deck,x)
        let modDeck = {Card = card; Deck = removeCardFromDeck deck x}
        modDeck

    let showCard c =
        let rankString =
            match c.Rank with
            | Ace -> "Ace (1 or 14)"
            | King -> "King (13)"
            | Queen -> "Queen (12)"
            | Jack -> "Jack (11)"
            | Value n -> string n
        let suitString = 
            match c.Suit with
            | Spades -> "Spades"
            | Clubs -> "Clubs"
            | Diamonds -> "Diamonds"
            | Hearts -> "Hearts"
        rankString + " of " + suitString

    let rankToInt(card)=
        let rankInt =
                 match card.Rank with
                 | King -> 13
                 | Queen -> 12
                 | Jack -> 11
                 | Value n -> int n
                 | Ace -> 14
        rankInt


    let handSum(cardRank, sum)=
        let mutable wholeSum = 0
        if cardRank = 14 then if cardRank + sum > 21 then wholeSum <- 1 + sum
                              else wholeSum <- 14 + sum
        else wholeSum <- cardRank + sum
        wholeSum

    let handOfPlayer(card)=
        let mutable phand:List<Card> = []
        phand <- card :: phand
        phand

    let listTest(deck:List<Card>)=
        let mutable printCard= []
        for i in 0..deck.Length-1 do
            printCard <- showCard(deck.Item(i)) :: printCard
        printCard


[<EntryPoint>]
    let main argv = 
        printfn "Welcome to Ventti!"
        printf "What is your name? "
        let ans = Console.ReadLine()
        printf "Hello %s. My name is Ville the Ventti banker. \nNow that we know each other, let's start playing!\nPress any key to continue." ans
        Console.ReadLine()
        ans = Console.ReadLine()

        //Creating the full deck
        let mutable fullDeck = createFullDeck()
        let mutable playersHand:List<Card> = []
        let mutable currentDeck = dealNewCard(fullDeck)
        playersHand <- currentDeck.Card :: playersHand
        let mutable nextCard = currentDeck.Card
        let mutable summa = rankToInt(nextCard)
        //let mutable stringi = showCard(currentDeck.Card)
        let mutable checkSum = handSum(summa, 0)
        //printfn "%s" stringi
        
        let cardHand = handOfPlayer(nextCard)

        fullDeck <- currentDeck.Deck
        currentDeck <- dealNewCard(fullDeck)
        playersHand <- currentDeck.Card :: playersHand
        nextCard <- currentDeck.Card
        summa <- rankToInt(nextCard)
        //stringi <- showCard(nextCard)
        checkSum <- handSum(summa, checkSum)
        // "%s" stringi
        printfn "%i" checkSum

        
        
        let printHand:List<string> = listTest(playersHand)
        for i in 0..printHand.Length-1 do
            let mutable currItem = printHand.Item(i)
            printfn "%s" currItem
 
        
        
        
        0 
