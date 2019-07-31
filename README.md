**1. Distance functions**

I programmed Minkowski, Canberra and Mahalanobis distance functions. Minkowski distance
value was changing when using different values for p. When the p=1(Manhattan) then Minkowski
distance was longest. When p value grew then the distance got shorter. That was expected result
because Manhattan can only move in one dimension at the time. Although choosing which one
to use depends on many things like dimensions and problem itself etc. Programmed my own
function for calculating covariance matrix.

**2. Clustering**

I programmed k-means algorithm. And then I programmed my own silhouette coefficient and
intra- to inter- cluster distances by myself to evaluate my clustering. Comparing different distance
functions then regardless of configuration, Canberra distance was always very different from the
others. But taking into account silhouette coefficient and intra- to inter functions then I got
expected results. When clusters were far from each other then silhouette was higher and intrato inter was smaller.
When I compared my programmed k - means to third party k – means algorithm then results
were exactly the same. 

**3. Classification**

I tested my knn algorithm with test points from our data generator. So I knew initially what was
their label. I then gave my new points one by one to knn algorithm and then when their initial
label was not the same as the label my algorithm got them I printed it out as triangle and when I
evaluated my algorithm with different distance algorithms then I compared them by count of
triangles.
With two dimensions there was not really difference between the distance functions. Maybe
Canberra was a bit longer than the others but not much. The main things that changed the
triangle count were: how far classes were and the data point count but this is logical finding of
course. A bit changed the result the neighbours count but that change was really slow. For
example with 60 neighbours the mistake was 16 and with 5 neighbours the mistake were 23.
But with three dimensions it showed clearly that mahlanobis distance was the best with each
configuration. 

**4. Fisher score**

Programmed Fisher score. Fisher score shows which dimensions are most relevant.

**5. Lof**

I programmed local outlier factor.
In my code I printed out all points that’s lof score was over 1,5 in red. Points that’s lof score was
over 1,2 in yellow and others in green. The further the point was from the center the bigger the
lof score was.