

g.fit = bn.fit(pc1.mmhc,pc1.disc.data)

g.pred = predict(g.fit,pc1.test.data,method="bayes-lw",node="L")

print( table(g.pred, pc1.test.data[,length(pc1.test.data)] )  ) #output the prediction matrix


## discrete Bayesian network (it is the same with ordinal nodes).
data(learning.test)
fitted = bn.fit(hc(learning.test), learning.test)
# the result should be around 0.025.
cpquery(fitted, (B == "b"), (A == "a"))
# for a single observation, predict the value of a single
# variable conditional on the others.
var = names(learning.test)
obs = 2
str = paste("(", names(learning.test)[-3], "=='",
            sapply(learning.test[obs,-3], as.character), "')",
            sep = "", collapse = " & ")
str
str2 = paste("(", names(learning.test)[3], "=='",
             as.character(learning.test[obs, 3]), "')", sep = "")
str2
cpquery(fitted, eval(parse(text = str2)), eval(parse(text = str)))
# do the same with likelihood weighting
cpquery(fitted, event = eval(parse(text = str2)),
        evidence = as.list(learning.test[2, -3]), method = "lw")
# conditional distribution of A given C == "c".
table(cpdist(fitted, "A", (C == "c")))

