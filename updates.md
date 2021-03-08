# Updates

This is a summary of updates to the algorithms (or updates to the code that affect the algorithms) since Feb 2021.

[IN PROGRESS, will add before/after plot diagrams]


### Commit 1 - Feb 11, 2021

- Fix range of days used in fit (endpoints were exclusive, now they are inclusive, consistent with ti_limits and tm_limits).
- Gaussian returns no fit if calculated ti < 1 (previously it was ti < 0).


### Commit 2 - Feb 28, 2021

- Fix the way gaussian amplitude and magnitude are calculated (amp_real = highest point between ti and tt, amp_fit = peak of curve)
- Merge gaussian amplitude and magnitude into one for asymmetric case (rather than left/right separate)
- amplitude = using a higher-res fitted curve, not just the fitted curve on days with valid data (which could be inaccurate if data is missing) - same with magnitude
- AMP/MAG ARE CALCULATED IN LINEAR SPACE, NOT LOG (even if the fit is done in log space)

[plot with expanded x-axis and more tickmarks, for amplitude/magnitude before/after comparison]


### Commit 3 - Mar 2, 2021

- fix asymmetric background chla calculation - diagonal from ti to tt


### Commit 4 - Mar 8, 2021

- if there is no valid data point at the calculated ti or tt of the shifted gaussian, use a linearly interpolated point to calculate magnitude_real
- fix asymmetric amplitude_fit glitch where it chooses highest point of one side or the other but maybe not the highest overall


**CHANGES PULLED TO DMAPPS ON MAR 8, 2021**
<hr>


