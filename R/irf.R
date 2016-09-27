"irf" <-
function(x, impulse=NULL, response=NULL, n.ahead=10, ortho=TRUE, cumulative=FALSE, boot=TRUE, ci=0.95, runs=100, seed=NULL, ...){
  UseMethod("irf", x)
}
