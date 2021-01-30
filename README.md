# tautology-checker
Tautology checker made on OCaml language

It checks wether a given proposition (Data types for propositions are shown in sthe ource file) is a tautology (always true) or not.

The function is_tau returns true if the given proposition is a tautology, and false in other case. This is implemented using the Semantic Tableaux method (https://en.wikipedia.org/wiki/Method_of_analytic_tableaux) instead of the Truth Table method (https://en.wikipedia.org/wiki/Truth_table). The Truth table method has a computational complexity of 2^n, where n is the number of unique variables. For a large number of variables, the Truth Table method is not viable.Therefore Semantic Tableaux method should be used,since it is a better solution.
