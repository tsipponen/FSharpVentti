open System

    exception StupidityError of string

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

                static member getAces() =
                 [yield Ace]

    type Card = {Suit: Suit; Rank: Rank}

    type origDeck = {
                    Card: Card 
                    Deck: List<Card>}

    type handAndDeckAndVenttiLists = {
                    Deck: List<Card>
                    Hand: List<Card>
                    Ventti: List<string>}

    //Creates a 52-card deck and returns it
    let createFullDeck() =
        let mutable fullDeck:List<Card> = []
        for suit in [Spades;Clubs;Diamonds;Hearts] do
            for rank in Rank.GetAllRanks() do
                fullDeck <- {Rank = rank; Suit = suit} :: fullDeck
        fullDeck

    // Returns the i:th card of a card list
    let getCard (deck:List<Card>, i) = deck.Item(i)
    
    //Folds a card and returns the remaining deck without the folded card
    let removeCardFromDeck (deck:List<Card>) i =
        let mutable remainingDeck:List<Card> = []
        for j in 0..i-1 do
            remainingDeck <- deck.Item(j) :: remainingDeck
        for j in i+1..deck.Length-1 do
            remainingDeck <- deck.Item(j) :: remainingDeck
        remainingDeck

    //Randomizes one card from the deck and returns the card and the remaining deck
    let newRandomCard(deck:List<Card>)=
        let rnd = System.Random()
        let seedRnd = System.Random()
        let seed = seedRnd.Next() % deck.Length
        let x = rnd.Next(seed, deck.Length)
        let card = getCard(deck,x)
        let modDeck = {Card = card; Deck = removeCardFromDeck deck x}
        modDeck

    //Matches a card's rank and suit and returns a string containing both of them
    let showCard c =
        let rankString =
            match c.Rank with
            | Ace -> "Ace (1 or 11)"
            | King -> "King (10)"
            | Queen -> "Queen (10)"
            | Jack -> "Jack (10)"
            | Value n -> string n
        let suitString = 
            match c.Suit with
            | Spades -> "Spades"
            | Clubs -> "Clubs"
            | Diamonds -> "Diamonds"
            | Hearts -> "Hearts"
        rankString + " of " + suitString

    //Returns a card's rank as an integer
    let rankToInt(card)=
        let rankInt =
                 match card.Rank with
                 | King -> 10
                 | Queen -> 10
                 | Jack -> 10
                 | Value n -> int n
                 | Ace -> 11
        rankInt

    //Returns the sum of all the cards in a hand
    let checkHandSum(hand:List<Card>)=
        let mutable theSum = 0
        let mutable intList = []
        for i in 0..hand.Length-1 do
            intList <- rankToInt(hand.Item(i)) :: intList

        for i in 0..intList.Length-1 do
            theSum <- theSum + intList.Item(i)
        for i in 0..intList.Length-1 do
            if intList.Item(i) = 11 then
                if theSum > 21 then theSum <- theSum - 10                                            
        theSum

    //Returns a list containing all the cards in a hand
    let printHand(deck:List<Card>)=
        let mutable printCard= []
        for i in 0..deck.Length-1 do
            printCard <- showCard(deck.Item(i)) :: printCard
        printCard

    //Checks the user's input and returns an answer as an integer or, if an incorrect input, raises an error message
    let checkPressed()=
        let mutable answer = 0 
        let mutable always = true
        let mutable ans = ""
        while always do
            try
                ans <- Console.ReadLine()
                if ans = "h" then answer <- 1
                if ans = "s" then answer <- 2
                if answer = 0 then raise (StupidityError("Press 'h' to hit or 's' to stand!"))
                always <- false
            with
                | StupidityError(err) -> printfn "%s" err
        answer

    //Returns the remaining deck, the updated hand and a list containing names of users that have a Ventti.
    let deal(deck:List<Card>, hand:List<Card>, isVentti, playerOrBanker, venttiList) =
            let mutable dekki = deck
            let mutable handu = hand
            let mutable isVenttiList = venttiList
            let mutable currentDeck = newRandomCard(dekki)
            let mutable nextCard = currentDeck.Card
            handu <- nextCard :: handu
            let mutable totalSum = checkHandSum(handu)
            if hand.IsEmpty then do
                dekki <- currentDeck.Deck
                currentDeck <- newRandomCard(dekki)
                nextCard <- currentDeck.Card
                handu <- nextCard :: handu
                totalSum <- checkHandSum(handu)
            printfn "%s's hand looks like this:\n" playerOrBanker
            let mutable printHandu:List<string> = printHand(handu)
            for i in 0..handu.Length-1 do
                let mutable currCard = printHandu.Item(i)
                printfn "%s" currCard
            printfn "With the total sum of %i\n" totalSum
            if totalSum = 21 then do isVenttiList <- playerOrBanker :: isVenttiList
            dekki <- currentDeck.Deck
            let returnable = {Deck = dekki; Hand = handu; Ventti = isVenttiList}
            returnable

    //Compares the hands of the player and the banker and declares the winner
    let compareHands(playerHand:List<Card>, bankerHand:List<Card>, playerName, banker) =
        let mutable pHand = checkHandSum(playerHand)
        let mutable bHand = checkHandSum(bankerHand)
        if bHand <= 21 && pHand <= 21 then do
            if pHand > bHand then do
                printfn "%s won! %s: %i, banker: %i" playerName playerName pHand bHand
                Console.ReadLine()
                Environment.Exit 1
            elif bHand > pHand then do
                printfn "%s won! %s: %i, banker: %i" banker playerName pHand bHand 
                Console.ReadLine()
                Environment.Exit 1
            else do
                printfn "It's a tie, so unfortunately the banker wins... %s: %i, banker: %i. Tough luck, %s" playerName pHand bHand playerName
                Console.ReadLine()
                Environment.Exit 1
        elif bHand > 21 && pHand <= 21 then do
            printfn "%s won! %s: %i, banker: %i" playerName playerName pHand bHand
            Console.ReadLine()
            Environment.Exit 1
        elif pHand > 21 && bHand <= 21 then do
            printfn"%s won... %s: %i, banker: %i" banker playerName pHand bHand
            Console.ReadLine()
            Environment.Exit 1
        
            


[<EntryPoint>]
    let main argv = 
        printfn "Welcome to Ventti!"
        printf "What is your name? "
        let name = Console.ReadLine()
        printfn "Hello %s. I am the Ventti banker. \nLet's start playing!\nPress Enter to get your first two cards.\n" name
        Console.ReadLine()

        //First deal to player
        let mutable fullDeck = createFullDeck()
        let mutable playersHand:List<Card> = []
        let mutable venttiList: List<string> = []
        let mutable afterDeal = deal(fullDeck, playersHand, true, name, venttiList)
        playersHand <- afterDeal.Hand
        fullDeck <- afterDeal.Deck
        venttiList <- afterDeal.Ventti

        //First deal to banker
        let mutable banker = "The banker"
        let mutable dealersHand:List<Card> = []
        afterDeal <- deal(fullDeck, dealersHand, true, banker, venttiList)
        dealersHand <- afterDeal.Hand
        fullDeck <- afterDeal.Deck
        venttiList <- afterDeal.Ventti

        if checkHandSum(playersHand) > checkHandSum(dealersHand) && checkHandSum(dealersHand) > 17 then do
            compareHands(playersHand, dealersHand, name, banker)

        //Continue?
        if checkHandSum(playersHand) = 21 = false then do
            printfn "Hit (press h) or stand (press s), %s?" name
            let mutable hitOrStand = checkPressed()

        //Hit (deal a new card to hand)
            while hitOrStand = 1 && checkHandSum(playersHand) < 21 do
                if checkHandSum(playersHand) > checkHandSum(dealersHand) && checkHandSum(dealersHand) > 17 then do
                    compareHands(playersHand, dealersHand, name, banker)
                else do
                    afterDeal <- deal(fullDeck, playersHand, false, name, venttiList)
                    playersHand <- afterDeal.Hand
                    fullDeck <- afterDeal.Deck
                    venttiList <- afterDeal.Ventti
                    if checkHandSum(playersHand) < 21 then do
                        hitOrStand <- checkPressed()
                    elif checkHandSum(playersHand) = 21 then do
                        hitOrStand <- 2
                //TAI:
                //printfn "%s got a Ventti and won the game!" name
                    else do
                        printfn "You went over 21, %s. You lost..." name
                        Console.ReadLine()
                        Environment.Exit 1
        else do 
            let mutable hitOrStand = 2
            printfn "%s got a Ventti, nice! toka" name
        let mutable hitOrStand = 2

        //Stand (stop picking cards)
        if hitOrStand = 2 then do
            let mutable dealerSum = checkHandSum(dealersHand)
            while dealerSum < 17 do 
                afterDeal <- deal(fullDeck, dealersHand, false, banker, venttiList)
                dealersHand <- afterDeal.Hand
                fullDeck <- afterDeal.Deck
                venttiList <- afterDeal.Ventti
                dealerSum <- checkHandSum(dealersHand)
                if checkHandSum(playersHand) > dealerSum && dealerSum > 17 then do
                    compareHands(playersHand, dealersHand, name, banker)
            compareHands(playersHand, dealersHand, name, banker)
        Console.ReadLine()
        0 
