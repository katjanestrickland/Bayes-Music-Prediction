# Bayes-Music-Prediction
With a small sample of discographies pulled from the Spotify API, I build two popularity prediction models based on musical key. First, a normal-normal mixture model, with both unique and fixed variance iterations. I use the EM algorithm to find maximum likelihood estimates for each unknown parameter. Second, in a hierarchical bayesian framework, where songs are nested within keys, I build credible intervals for a single band, specifically using the Gibbs sampler to draw samples from the posterior distribution. 

### Project Page

[Markdown Link](https://katjanewilson.github.io/Bayes-Music-Prediction/)



# citations
