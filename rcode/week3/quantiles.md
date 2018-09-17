    library(ismev)

    ## Loading required package: mgcv

    ## Loading required package: nlme

    ## This is mgcv 1.8-23. For overview type 'help("mgcv-package")'.

    data(wavesurge)
    sel <- order(abs(wavesurge$wave-6))[1:500]
    plot(ecdf(wavesurge$surge[sel]), xlab="Surge [m]", ylim=c(-0.05, 1), ylab=expression(P(Y<=y)), main="",xlim=range(wavesurge$surge[sel]))
    rug(wavesurge$surge[sel])
    taus <-  c(0.9)
    cols <- c(2)
    for (i in seq_along(taus)) {
        q <- quantile(wavesurge$surge[sel], taus[i])
        abline(h=taus[i], col=cols[i], lwd=2, lty=2)
        abline(v=q, col=cols[i], lwd=2)
        corners = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
        par(xpd = TRUE) #Draw outside plot area
        text(y = corners[4]+.01, x = q, paste0(100*taus[i],"%"), srt = 0, adj=c(0.5,0), col=cols[i])
        par(xpd=FALSE)
    }

<img src="quantiles_files/figure-markdown_strict/unnamed-chunk-1-1.png" width=".45\textwidth" />
