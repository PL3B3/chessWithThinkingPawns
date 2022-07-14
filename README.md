# chessWithThinkingPawns

### Synopsis
This project investigates how effective a genetic algorithm will be given different environmental conditions. Specifically, I simulate thousands of simple neural network agents playing a version of the classic [prisoner's dilemma](https://www.investopedia.com/articles/investing/110513/utilizing-prisoners-dilemma-business-and-economy.asp) game. 


### Prisoner's Dilemma
In this game, two players must decide whether to betray the other or remain loyal.
- If both players betray on another, they are both punished moderately. 
- If neither betrays the other, they are both punished lightly. 
- If only one player betrays the other, the player that was betrayed will be punished severely, while the betrayer is not punished. 

### How Agents Play the Game
In each iteration, there are 10 rounds. Within each round, agents are randomly paired with another and play 20 consecutive cycles of the game. The only information available to them is the outcome of the previous cycle.

### Experiment
Agents would "learn" to play the game through a [genetic algorithm](https://pub.towardsai.net/genetic-algorithm-ga-introduction-with-example-code-e59f9bc58eaf). In each iteration, agents are scored according to how well they did in the game. The higher the score, the more likely the agent will "pass on their genes" to the next generation of agents. Random mutations to neural network weights are occasionally introduced so that the agents will evolve.

I ran three versions of the experiment which differed on how "strict" the genetic algorithm was. A less strict genetic algorithm allows low-scoring agents to pass on their genes more frequently, while a more strict algorithm strongly prefers high-scorers. At the end of each experiment, I looked at what kinds of strategies the agents learned. In particular, I wanted to see how many agents learned the optimal prisoner's dilemma strategy (tit-for-tat).

### Results
Strangely enough, the less strict experiment actually produced the largest share of agents with the optimal strategy.
