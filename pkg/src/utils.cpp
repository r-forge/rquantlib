// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- 
//
// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright (C) 2002 - 2009 Dirk Eddelbuettel <edd@debian.org>
// Copyright (C) 2005 - 2006  Dominick Samperi
//
// $Id$
//
// This file is part of the RQuantLib library for GNU R.
// It is made available under the terms of the GNU General Public
// License, version 2, or at your option, any later version,
// incorporated herein by reference.
//
// This program is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied
// warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public
// License along with this program; if not, write to the Free
// Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA

#include "rquantlib.hpp"

// cf QuantLib-0.9.0/test-suite/europeanoption.cpp
boost::shared_ptr<VanillaOption>
makeOption(const boost::shared_ptr<StrikedTypePayoff>& payoff,
           const boost::shared_ptr<Exercise>& exercise,
           const boost::shared_ptr<Quote>& u,
           const boost::shared_ptr<YieldTermStructure>& q,
           const boost::shared_ptr<YieldTermStructure>& r,
           const boost::shared_ptr<BlackVolTermStructure>& vol,
           EngineType engineType,
           Size binomialSteps,
           Size samples) {
  
    boost::shared_ptr<GeneralizedBlackScholesProcess> stochProcess = makeProcess(u,q,r,vol);
    boost::shared_ptr<PricingEngine> engine;

    switch (engineType) {
    case Analytic:
        engine = boost::shared_ptr<PricingEngine>(new AnalyticEuropeanEngine(stochProcess));
        break;
    case JR:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<JarrowRudd>(stochProcess, binomialSteps));
        break;
    case CRR:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<CoxRossRubinstein>(stochProcess, binomialSteps));
    case EQP:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<AdditiveEQPBinomialTree>(stochProcess, binomialSteps));
        break;
    case TGEO:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<Trigeorgis>(stochProcess, binomialSteps));
        break;
    case TIAN:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<Tian>(stochProcess, binomialSteps));
        break;
    case LR:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<LeisenReimer>(stochProcess, binomialSteps));
        break;
    case JOSHI:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<Joshi4>(stochProcess, binomialSteps));
        break;
    case FiniteDifferences:
        engine = boost::shared_ptr<PricingEngine>(new FDEuropeanEngine(stochProcess, binomialSteps, samples));
        break;
    case Integral:
        engine = boost::shared_ptr<PricingEngine>(new IntegralEngine(stochProcess));
        break;
    case PseudoMonteCarlo:
        engine = MakeMCEuropeanEngine<PseudoRandom>(stochProcess)
            .withStepsPerYear(1)
            .withSamples(samples)
            .withSeed(42);
        break;
    case QuasiMonteCarlo:
        engine = MakeMCEuropeanEngine<LowDiscrepancy>(stochProcess)
            .withStepsPerYear(1)
            .withSamples(samples);
        break;
    default:
        QL_FAIL("Unknown engine type");
    }
    boost::shared_ptr<VanillaOption> option(new EuropeanOption(payoff, exercise));
    option->setPricingEngine(engine);
    return option;
}

// QuantLib option setup utils, copied from the test-suite sources

boost::shared_ptr<YieldTermStructure>
makeFlatCurve(const Date& today,
	      const boost::shared_ptr<Quote>& forward,
	      const DayCounter& dc) {
    return boost::shared_ptr<YieldTermStructure>(
	   new FlatForward(today, Handle<Quote>(forward), dc));
}

boost::shared_ptr<YieldTermStructure>
flatRate(const Date& today,
	 const boost::shared_ptr<Quote>& forward,
	 const DayCounter& dc) {
  return boost::shared_ptr<YieldTermStructure>(
	       new FlatForward(today, Handle<Quote>(forward), dc));
}
  
boost::shared_ptr<BlackVolTermStructure> 
makeFlatVolatility(const Date& today,
                   const boost::shared_ptr<Quote>& vol,
                   const DayCounter dc) {
    return boost::shared_ptr<BlackVolTermStructure>(
           new BlackConstantVol(today, NullCalendar(), Handle<Quote>(vol), dc));
}

boost::shared_ptr<BlackVolTermStructure>
flatVol(const Date& today,
	const boost::shared_ptr<Quote>& vol,
	const DayCounter& dc) {
  return boost::shared_ptr<BlackVolTermStructure>(new
            BlackConstantVol(today, NullCalendar(), Handle<Quote>(vol), dc));
}

boost::shared_ptr<GeneralizedBlackScholesProcess>
makeProcess(const boost::shared_ptr<Quote>& u,
            const boost::shared_ptr<YieldTermStructure>& q,
            const boost::shared_ptr<YieldTermStructure>& r,
            const boost::shared_ptr<BlackVolTermStructure>& vol) {
    return boost::shared_ptr<BlackScholesMertonProcess>(
           new BlackScholesMertonProcess(Handle<Quote>(u),
                                         Handle<YieldTermStructure>(q),
                                         Handle<YieldTermStructure>(r),
                                         Handle<BlackVolTermStructure>(vol)));
}

// R uses dates indexed to Jan 1, 1970. Rcpp uses an internal Julian Date representation,
// but Quantlib uses the 'spreadsheet' format indexed to 1905 so we need to adjust
int dateFromR(const RcppDate &d) {
    return(d.getJDN() - RcppDate::Jan1970Offset + RcppDate::QLtoJan1970Offset);
}
DayCounter getDayCounter(double n){
    if (n==0) return Actual360();
    else if (n==1) return Actual365Fixed();
    else if (n==2) return ActualActual();
    else if (n==3) return Business252();
    else if (n==4) return OneDayCounter();
    else if (n==5) return SimpleDayCounter();
    else  return Thirty360();
}
BusinessDayConvention getBusinessDayConvention(double n){
    if (n==0) return Following;
    else if (n==1) return ModifiedFollowing;
    else if (n==2) return Preceding;
    else if (n==3) return ModifiedPreceding;
    else  return Unadjusted;
}
Compounding getCompounding(double n){
    if (n==0) return Simple;
    else if (n==1) return Compounded;
    else if (n==2) return Continuous;
    else return SimpleThenCompounded;
}
Frequency getFrequency(double n){
    if (n==0) return NoFrequency;
    else if (n==1) return Once;
    else if (n==2) return Annual;
    else if (n==3) return Semiannual;
    else if (n==4) return EveryFourthMonth;
    else if (n==5) return Quarterly;
    else if (n==6) return Bimonthly;
    else if (n==7) return EveryFourthWeek;
    else if (n==8) return Biweekly;
    else if (n==9) return Weekly;
    else return Daily;
}
