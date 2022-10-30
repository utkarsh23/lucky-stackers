# Lucky Stackers
This game was created to learn clarity. Its a simple lottery game which requires exactly 6 users to pool in a minimum of 10 STX each and selects a user at random who receives the complete pool after subtracting game commision.

## How the game works
* Anyone can create a new game, which requires the user to send some amount of STX (min. 10 STX) to the lucky stackers smart contract. Let's call this the *game amount*.
* Once this game is created, 5 other users can join this game by sending this *game amount* set by the game creator to lucky stackers smart contract.
* Once the 6th user joins the game, the smart contract executes the lottery logic to pick a winner at random, calculate the game commission to be sent to the contract deployer and then send the remaining amount in the pool to the winner.
* If the game is still pending after 5 days, any one of the participants can end the game and all participants will get back the amount that they had pooled in for that game.

## Credits
This contract uses [@FriendsFerdinand](https://github.com/FriendsFerdinand)'s random number generation example from https://github.com/FriendsFerdinand/random-test
