# Binomial Model
This project contains essential computation and illustration functions for valuation of call and put options by well known binomial model. 
The whole project was created as an extended version of my university project on the same topic. 

## Contents
- 'Binomial model.R' – the main R script with all function.

## Description of Functions
binomial.eu.call() --- estimates the present value of an European call. 
binomial.eu.put() --- estimates the present value of an European put.
Key inputs:
  - 'r' – risk free interest rate (p.a.)
  - 's' – volatility rate of the underlying.
  - 's0' – current price of underlying.
  - 'E' – the exercise price.
  - 'T' – period of time to maturity date (in years).
  - 'n' – number of steps in the binomial tree.
  - 'position' – postion in the contract. Possible entries: 'long', 'short'.
  - 'show_payoff' – if 'TRUE', displays the payoff graph (incl. premium).

binomial.am.call() --- estimates the present value of an American call.
binomial.am.put() --- estimates the present value of an American put.
Additionally to the input from the previous functions, the latter 2 also have:
- 'div' – if not 'NULL', considers the given amount of dividends in the model.
- 'exdivdate' – assigns the ex-dividend date into function. Cannot be missed, if 'div' isn't 'NULL'.

binomial.path() --- helps model a sample binomial path given the input for an option.
                    The output contains all spot rates of underlying across the binomial tree as well as its pace from today till the maturity date.
