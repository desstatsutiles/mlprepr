# mlprepr

This library offers a set of functions to prepare any dataset easily, in order to use machine learning.

The main features include :
- Recoding categorical variables (including one hot encoding)
- Recoding boolean variables
- Dealing with extreme data (monovariate winsorization)

Moreover, its syntax is designed for production, which means you can apply the exact data preparation used on the train set to the test, or real-life set. No more unreliable data pipelines.

What does a project look like, then ?
- Learn a good data preparation (encode variables, etc...)
- Apply it to the training set 
- Lean a model on the train set (feature not provided by this library)
- Apply it to the train set
- Use model on the train set with 100% guarantee on data preparation

Additionaly, this library helps you detect unreliable variables (we call this drift) :
- Variables that are different in the train set than in the test set
- Variables having a distribution that depends on their id (ie position) in the training set

You can then discard these variables before training to make sure your model will work in production. Moreover, you can test it again before applying it in production to make sure the dataset is still identical to the one used during training.

Looks good to me.

Now if you're not sold yet, note that :
- All functions use data.table as a data structure, wich means it is very fast and memory efficient
- And that's it for now but I like lists
