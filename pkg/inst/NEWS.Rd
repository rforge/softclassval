\name{softclassval NEWS}
\title{softclassval News}
\encoding{UTF-8}

\section{Changes in Version 0.5}{
\subsection{User-visible changes}{
\itemize{
  \item function \code{factor2matrix} that computes membership matrices
  from a factor with class labels.
  \item \code{checkrp} tries to find out whether columns in reference
  and prediction are the same, and, if not throws a warning.
}
}
\subsection{Bug fixes}{
  \itemize{
	 \item \code{hardclasses} now accepts missing predictions and returns
  \code{NA} if no column has membership 1.
}
}
}
 

