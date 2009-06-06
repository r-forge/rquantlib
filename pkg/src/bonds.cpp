/* RQuantLib -- R interface to the QuantLib libraries
##
## Copyright (C) 2002 - 2009 Dirk Eddelbuettel <edd@debian.org>
## Copyright (C) 2009        Khanh Nguyen <knguyen@cs.umb.edu>
##
## $Id$
##
## This file is part of the RQuantLib library for GNU R.
## It is made available under the terms of the GNU General Public
## License, version 2, or at your option, any later version,
## incorporated herein by reference.
##
## This program is distributed in the hope that it will be
## useful, but WITHOUT ANY WARRANTY; without even the implied
## warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
## PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public
## License along with this program; if not, write to the Free
## Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
## MA 02111-1307, USA
*/

#include "rquantlib.hpp"
using namespace boost;


RcppExport  SEXP QL_ZeroPriceByYield(SEXP optionParameters) {
    SEXP rl = R_NilValue;
    char* exceptionMesg=NULL;
    try{
       RcppParams rparam(optionParameters);
       double yield = rparam.getDoubleValue("yield");
       double faceAmount = rparam.getDoubleValue("faceAmount");
       double dayCounter = rparam.getDoubleValue("dayCounter");
       double frequency = rparam.getDoubleValue("frequency");
       double businessDayConvention = rparam.getDoubleValue("businessDayConvention");
       double compound = rparam.getDoubleValue("compound");
       RcppDate mDate = rparam.getDateValue("maturityDate");
       RcppDate iDate = rparam.getDateValue("issueDate");
       QuantLib::Date maturityDate(dateFromR(mDate));
       QuantLib::Date issueDate(dateFromR(iDate));
       //setup bond
       QuantLib::Integer fixingDays = 2;
       Calendar calendar=UnitedStates(UnitedStates::GovernmentBond);
       Date todaysDate = calendar.advance(issueDate, -fixingDays, Days);
       Settings::instance().evaluationDate() = todaysDate;
       Natural settlementDays = 1;
       
       BusinessDayConvention bdc = getBusinessDayConvention(businessDayConvention);
       double redemption = 100;
       ZeroCouponBond zbond(settlementDays, calendar,
                            faceAmount, maturityDate,
                            bdc, redemption, issueDate);
       
       //return cleanPrice
       RcppResultSet rs;
       DayCounter dc = getDayCounter(dayCounter);
       Compounding cp = getCompounding(compound);
       Frequency freq = getFrequency(frequency);
       rs.add("cleanPrice", zbond.cleanPrice(yield, dc, cp, freq));
       rl = rs.getReturnList();
    } catch(std::exception& ex) {
        exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
        exceptionMesg = copyMessageToR("unknown reason");
    }
    
    if(exceptionMesg != NULL)
        error(exceptionMesg);
    
    return rl;
}

RcppExport  SEXP QL_ZeroYield(SEXP optionParameters) {
    SEXP rl = R_NilValue;
    char* exceptionMesg=NULL;
    try{
       RcppParams rparam(optionParameters);
       double price = rparam.getDoubleValue("price");
       double faceAmount = rparam.getDoubleValue("faceAmount");
       double dayCounter = rparam.getDoubleValue("dayCounter");
       double frequency = rparam.getDoubleValue("frequency");
       double businessDayConvention = rparam.getDoubleValue("businessDayConvention");
       double compound = rparam.getDoubleValue("compound");
       RcppDate mDate = rparam.getDateValue("maturityDate");
       RcppDate iDate = rparam.getDateValue("issueDate");
       QuantLib::Date maturityDate(dateFromR(mDate));
       QuantLib::Date issueDate(dateFromR(iDate));
       //setup bond
       QuantLib::Integer fixingDays = 2;
       Calendar calendar=UnitedStates(UnitedStates::GovernmentBond);
       Date todaysDate = calendar.advance(issueDate, -fixingDays, Days);
       Settings::instance().evaluationDate() = todaysDate;
       Natural settlementDays = 1;
       
       BusinessDayConvention bdc = getBusinessDayConvention(businessDayConvention);
       double redemption = 100;
       ZeroCouponBond zbond(settlementDays, calendar,
                            faceAmount, maturityDate,
                            bdc, redemption, issueDate);
       
       //return yield
       RcppResultSet rs;
       DayCounter dc = getDayCounter(dayCounter);
       Compounding cp = getCompounding(compound);
       Frequency freq = getFrequency(frequency);
       rs.add("yield", zbond.yield(price, dc, cp, freq));
       rl = rs.getReturnList();
    } catch(std::exception& ex) {
        exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
        exceptionMesg = copyMessageToR("unknown reason");
    }
    
    if(exceptionMesg != NULL)
        error(exceptionMesg);
    
    return rl;
}


SEXP ZeroBond(SEXP bond, 
              Handle<YieldTermStructure> &discountCurve,
              SEXP dateparams) {

    SEXP rl=R_NilValue;
    char* exceptionMesg=NULL;
    try{
        RcppParams rparam(bond);
        double faceAmount = rparam.getDoubleValue("faceAmount");
        RcppDate mDate = rparam.getDateValue("maturityDate");
        RcppDate iDate = rparam.getDateValue("issueDate");
        QuantLib::Date maturityDate(dateFromR(mDate));
        QuantLib::Date issueDate(dateFromR(iDate));
        double redemption = rparam.getDoubleValue("redemption");

        RcppParams misc(dateparams);
        double settlementDays = misc.getDoubleValue("settlementDays");
        std::string cal = misc.getStringValue("calendar");
        double businessDayConvention = misc.getDoubleValue("businessDayConvention");
      

        
        /*
          test-suite/bonds.cpp
        */      

        //set up BusinessDayConvetion
        BusinessDayConvention bdc = getBusinessDayConvention(businessDayConvention);
        
        //set up calendar
        Calendar calendar = UnitedStates(UnitedStates::GovernmentBond);
        if (cal == "us"){
            calendar = UnitedStates(UnitedStates::GovernmentBond);
        }
        else if (cal == "uk"){
            calendar = UnitedKingdom(UnitedKingdom::Exchange);
        }
        
        
        ZeroCouponBond bond(settlementDays,
                            calendar,
                            faceAmount,
                            maturityDate,
                            bdc,
                            redemption, issueDate);
        
        boost::shared_ptr<PricingEngine> bondEngine(
                                                    new DiscountingBondEngine(discountCurve));
        bond.setPricingEngine(bondEngine);

        //cashflow
        int numCol = 2;
        std::vector<std::string> colNames(numCol);
        colNames[0] = "Date";
        colNames[1] = "Amount";
        RcppFrame frame(colNames);
        
        Leg bondCashFlow = bond.cashflows();
        for (unsigned int i = 0; i< bondCashFlow.size(); i++){
            std::vector<ColDatum> row(numCol);
            Date d = bondCashFlow[i]->date();
            row[0].setDateValue(RcppDate(d.month(), d.dayOfMonth(), d.year()));
            row[1].setDoubleValue(bondCashFlow[i]->amount());
            frame.addRow(row);
        }

        
        RcppResultSet rs;
        rs.add("NPV", bond.NPV());
        rs.add("cleanPrice", bond.cleanPrice());
        rs.add("dirtyPrice", bond.dirtyPrice());
        rs.add("accruedCoupon", bond.accruedAmount());
        rs.add("yield", bond.yield(Actual360(), Compounded, Annual));
        rs.add("cashFlow", frame);
        rl = rs.getReturnList();
    } catch(std::exception& ex) {
        exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
        exceptionMesg = copyMessageToR("unknown reason");
    }
    
    if(exceptionMesg != NULL)
        error(exceptionMesg);
    
    return rl;
}

RcppExport SEXP QL_ZBond1(SEXP bond, SEXP discountCurve, SEXP dateparams){
    SEXP rl = R_NilValue;
    char *exceptionMesg = NULL;
    try{
        RcppParams curve(discountCurve);
        Rate riskFreeRate = curve.getDoubleValue("riskFreeRate");
        RcppDate today_Date = curve.getDateValue("todayDate");       
        QuantLib::Date today(dateFromR(today_Date));

        boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(riskFreeRate));
        Settings::instance().evaluationDate() = today;
        Handle<YieldTermStructure> discountCurve(flatRate(today,rRate,Actual360()));

        rl = ZeroBond(bond, discountCurve, dateparams);
    } catch(std::exception& ex) {
        exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
        exceptionMesg = copyMessageToR("unknown reason");
    }    
    if(exceptionMesg != NULL)
        error(exceptionMesg);
    
    return rl;
}
RcppExport SEXP QL_ZBond2(SEXP bond, SEXP params, 
                          SEXP tsQuotes, SEXP times,
                          SEXP dateparams){
    SEXP rl = R_NilValue;
    char *exceptionMesg = NULL;
    try{
        std::cout << "in zbond2";
        Handle<YieldTermStructure> discountCurve(
                                 buildTermStructure(params, tsQuotes, times));
        std::cout << discountCurve->discount(10);
        rl = ZeroBond(bond, discountCurve, dateparams);
    } catch(std::exception& ex) {
        exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
        exceptionMesg = copyMessageToR("unknown reason");
    }    
    if(exceptionMesg != NULL)
        error(exceptionMesg);   
    return rl;
}


RcppExport  SEXP QL_ZeroCouponBondCustomCurve(SEXP optionParameters,
                                              SEXP params, SEXP tsQuotes,
                                              SEXP times) {

    SEXP rl=R_NilValue;
    char* exceptionMesg=NULL;
    try{
        RcppParams rparam(optionParameters);
        double settlementDays = rparam.getDoubleValue("settlementDays");
        std::string cal = rparam.getStringValue("calendar");
        double faceAmount = rparam.getDoubleValue("faceAmount");
        double businessDayConvention = rparam.getDoubleValue("businessDayConvention");
        double redemption = rparam.getDoubleValue("redemption");
        
        RcppDate mDate = rparam.getDateValue("maturityDate");
        RcppDate iDate = rparam.getDateValue("issueDate");
        
        QuantLib::Date maturityDate(dateFromR(mDate));
        QuantLib::Date issueDate(dateFromR(iDate));
        
        
        /*
          Build the discount curve
        */
        Handle<YieldTermStructure> curve(
                                         buildTermStructure(params, tsQuotes, times));
        
        //set up BusinessDayConvetion
        BusinessDayConvention bdc = getBusinessDayConvention(businessDayConvention);
        
        //set up calendar
        Calendar calendar = UnitedStates(UnitedStates::GovernmentBond);
        if (cal == "us"){
            calendar = UnitedStates(UnitedStates::GovernmentBond);
        }
        else if (cal == "uk"){
            calendar = UnitedKingdom(UnitedKingdom::Exchange);
        }
        
        
        ZeroCouponBond bond(settlementDays,
                            calendar,
                            faceAmount,
                            maturityDate,
                            bdc,
                            redemption, issueDate);
        
        boost::shared_ptr<PricingEngine> bondEngine(
                                                    new DiscountingBondEngine(curve));
        bond.setPricingEngine(bondEngine);

        //cashflow
        int numCol = 2;
        std::vector<std::string> colNames(numCol);
        colNames[0] = "Date";
        colNames[1] = "Amount";
        RcppFrame frame(colNames);
        
        Leg bondCashFlow = bond.cashflows();
        for (unsigned int i = 0; i< bondCashFlow.size(); i++){
            std::vector<ColDatum> row(numCol);
            Date d = bondCashFlow[i]->date();
            row[0].setDateValue(RcppDate(d.month(), d.dayOfMonth(), d.year()));
            row[1].setDoubleValue(bondCashFlow[i]->amount());
            frame.addRow(row);
        }
        
        RcppResultSet rs;
        rs.add("NPV", bond.NPV());
        rs.add("cleanPrice", bond.cleanPrice());
        rs.add("dirtyPrice", bond.dirtyPrice());
        rs.add("accruedCoupon", bond.accruedAmount());
        rs.add("yield", bond.yield(Actual360(), Compounded, Annual));
        rs.add("cashFlow", frame);

        rl = rs.getReturnList();
    } catch(std::exception& ex) {
        exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
        exceptionMesg = copyMessageToR("unknown reason");
    }
    
    if(exceptionMesg != NULL)
        error(exceptionMesg);
    
    return rl;
}


RcppExport  SEXP QL_FixedRateBond(SEXP optionParameters, SEXP ratesVec) {
  
    SEXP rl=R_NilValue;
    char* exceptionMesg=NULL;
    try{
        RcppParams rparam(optionParameters);
        double settlementDays = rparam.getDoubleValue("settlementDays");
        std::string cal = rparam.getStringValue("calendar");
        double faceAmount = rparam.getDoubleValue("faceAmount");
        double businessDayConvention = rparam.getDoubleValue("businessDayConvention");
        double redemption = rparam.getDoubleValue("redemption");
        double dayCounter = rparam.getDoubleValue("dayCounter");
        Rate riskFreeRate = rparam.getDoubleValue("riskFreeRate");
        double frequency = rparam.getDoubleValue("period");
        
        RcppDate mDate = rparam.getDateValue("maturityDate");
        RcppDate eDate = rparam.getDateValue("effectiveDate");
        RcppDate iDate = rparam.getDateValue("issueDate");
        RcppDate today_Date = rparam.getDateValue("todayDate");       
        QuantLib::Date maturityDate(dateFromR(mDate));
        QuantLib::Date effectiveDate(dateFromR(eDate));
        QuantLib::Date issueDate(dateFromR(iDate));
        QuantLib::Date today(dateFromR(today_Date));
        
        //extract coupon rates vector
        RcppVector<double> RcppVec(ratesVec); 
        std::vector<double> rates(RcppVec.stlVector());
        
        //set up BusinessDayConvetion
        BusinessDayConvention bdc = getBusinessDayConvention(businessDayConvention);
        DayCounter dc = getDayCounter(dayCounter);
        Frequency freq = getFrequency(frequency);

     
        //set up calendar
        Calendar calendar = UnitedStates(UnitedStates::GovernmentBond);
        if (cal == "us"){
            calendar = UnitedStates(UnitedStates::GovernmentBond);
        }
        else if (cal == "uk"){
            calendar = UnitedKingdom(UnitedKingdom::Exchange);
        }
        
        boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(riskFreeRate));
        Settings::instance().evaluationDate() = today;
        Handle<YieldTermStructure> discountCurve(flatRate(today,rRate,Actual360()));
        
        //build the bond
        Schedule sch(effectiveDate, maturityDate,
                     Period(freq), calendar,
                     bdc, bdc, DateGeneration::Backward, false);
        
        FixedRateBond bond(settlementDays, faceAmount, sch,
                           rates,dc, bdc, redemption, issueDate);
        
        
        //bond price
        boost::shared_ptr<PricingEngine> bondEngine(
                                                    new DiscountingBondEngine(discountCurve));
        bond.setPricingEngine(bondEngine);
        
        //cashflow
        int numCol = 2;
        std::vector<std::string> colNames(numCol);
        colNames[0] = "Date";
        colNames[1] = "Amount";
        RcppFrame frame(colNames);
        
        Leg bondCashFlow = bond.cashflows();
        for (unsigned int i = 0; i< bondCashFlow.size(); i++){
            std::vector<ColDatum> row(numCol);
            Date d = bondCashFlow[i]->date();
            row[0].setDateValue(RcppDate(d.month(), d.dayOfMonth(), d.year()));
            row[1].setDoubleValue(bondCashFlow[i]->amount());
            frame.addRow(row);
        }
        
        
        RcppResultSet rs;

        rs.add("NPV", bond.NPV());
        rs.add("cleanPrice", bond.cleanPrice());
        rs.add("dirtyPrice", bond.dirtyPrice());
        rs.add("accruedCoupon", bond.accruedAmount());
        rs.add("yield", bond.yield(Actual360(), Compounded, Annual));
        rs.add("cashFlow", frame);
        rl = rs.getReturnList();
        
    } catch(std::exception& ex) {
        exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
        exceptionMesg = copyMessageToR("unknown reason");
    }
    
    if(exceptionMesg != NULL)
        error(exceptionMesg);
    
    return rl;
}
RcppExport  SEXP QL_FixedRateBondCustomCurve(SEXP optionParameters, SEXP ratesVec,
                                    SEXP params, SEXP tsQuotes, SEXP times) {
  
    SEXP rl=R_NilValue;
    char* exceptionMesg=NULL;
    try{
        RcppParams rparam(optionParameters);
        double settlementDays = rparam.getDoubleValue("settlementDays");
        std::string cal = rparam.getStringValue("calendar");
        double faceAmount = rparam.getDoubleValue("faceAmount");
        double businessDayConvention = rparam.getDoubleValue("businessDayConvention");
        double redemption = rparam.getDoubleValue("redemption");
        double dayCounter = rparam.getDoubleValue("dayCounter");        
        double frequency = rparam.getDoubleValue("period");
        
        RcppDate mDate = rparam.getDateValue("maturityDate");
        RcppDate eDate = rparam.getDateValue("effectiveDate");
        RcppDate iDate = rparam.getDateValue("issueDate");             
        QuantLib::Date maturityDate(dateFromR(mDate));
        QuantLib::Date effectiveDate(dateFromR(eDate));
        QuantLib::Date issueDate(dateFromR(iDate));       
        
        //extract coupon rates vector
        RcppVector<double> RcppVec(ratesVec); 
        std::vector<double> rates(RcppVec.stlVector());
        
        //set up BusinessDayConvetion
        BusinessDayConvention bdc = getBusinessDayConvention(businessDayConvention);
        DayCounter dc = getDayCounter(dayCounter);
        Frequency freq = getFrequency(frequency);

     
        //set up calendar
        Calendar calendar = UnitedStates(UnitedStates::GovernmentBond);
        if (cal == "us"){
            calendar = UnitedStates(UnitedStates::GovernmentBond);
        }
        else if (cal == "uk"){
            calendar = UnitedKingdom(UnitedKingdom::Exchange);
        }

 /*
          Build the discount curve
        */
        Handle<YieldTermStructure> discountCurve(
                                         buildTermStructure(params, tsQuotes, times));        

        
        //build the bond
        Schedule sch(effectiveDate, maturityDate,
                     Period(freq), calendar,
                     bdc, bdc, DateGeneration::Backward, false);
        
        FixedRateBond bond(settlementDays, faceAmount, sch,
                           rates,dc, bdc, redemption, issueDate);
        
        
        //bond price
        boost::shared_ptr<PricingEngine> bondEngine(
                                                    new DiscountingBondEngine(discountCurve));
        bond.setPricingEngine(bondEngine);
        
        //cashflow
        int numCol = 2;
        std::vector<std::string> colNames(numCol);
        colNames[0] = "Date";
        colNames[1] = "Amount";
        RcppFrame frame(colNames);
        
        Leg bondCashFlow = bond.cashflows();
        for (unsigned int i = 0; i< bondCashFlow.size(); i++){
            std::vector<ColDatum> row(numCol);
            Date d = bondCashFlow[i]->date();
            row[0].setDateValue(RcppDate(d.month(), d.dayOfMonth(), d.year()));
            row[1].setDoubleValue(bondCashFlow[i]->amount());
            frame.addRow(row);
        }
        
        
        RcppResultSet rs;

        rs.add("NPV", bond.NPV());
        rs.add("cleanPrice", bond.cleanPrice());
        rs.add("dirtyPrice", bond.dirtyPrice());
        rs.add("accruedCoupon", bond.accruedAmount());
        rs.add("yield", bond.yield(Actual360(), Compounded, Annual));
        rs.add("cashFlow", frame);
        rl = rs.getReturnList();
        
    } catch(std::exception& ex) {
        exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
        exceptionMesg = copyMessageToR("unknown reason");
    }
    
    if(exceptionMesg != NULL)
        error(exceptionMesg);
    
    return rl;
}

 
RcppExport  SEXP QL_FixedRateBondYield(SEXP optionParameters, SEXP ratesVec) {
  
    SEXP rl=R_NilValue;
    char* exceptionMesg=NULL;
    try{
        RcppParams rparam(optionParameters);
        double settlementDays = rparam.getDoubleValue("settlementDays");
        std::string cal = rparam.getStringValue("calendar");
        double price = rparam.getDoubleValue("price");
        double faceAmount = rparam.getDoubleValue("faceAmount");
        double businessDayConvention = rparam.getDoubleValue("businessDayConvention");
        double compound = rparam.getDoubleValue("compound");
        double redemption = rparam.getDoubleValue("redemption");
        double dayCounter = rparam.getDoubleValue("dayCounter");
        double frequency = rparam.getDoubleValue("period");
        
        RcppDate mDate = rparam.getDateValue("maturityDate");
        RcppDate eDate = rparam.getDateValue("effectiveDate");
        RcppDate iDate = rparam.getDateValue("issueDate");
        QuantLib::Date maturityDate(dateFromR(mDate));
        QuantLib::Date effectiveDate(dateFromR(eDate));
        QuantLib::Date issueDate(dateFromR(iDate));
        
        //extract coupon rates vector
        RcppVector<double> RcppVec(ratesVec); 
        std::vector<double> rates(RcppVec.stlVector());
        
        //set up BusinessDayConvetion
        BusinessDayConvention bdc = getBusinessDayConvention(businessDayConvention);
        DayCounter dc = getDayCounter(dayCounter);
        Frequency freq = getFrequency(frequency);
        Compounding cp = getCompounding(compound);
 
        //set up calendar
        Calendar calendar = UnitedStates(UnitedStates::GovernmentBond);
        if (cal == "us"){
            calendar = UnitedStates(UnitedStates::GovernmentBond);
        }
        else if (cal == "uk"){
            calendar = UnitedKingdom(UnitedKingdom::Exchange);
        }
        
        //build the bond
        Schedule sch(effectiveDate, maturityDate,
                     Period(freq), calendar,
                     bdc, bdc, DateGeneration::Backward, false);
        
        FixedRateBond bond(settlementDays, faceAmount, sch,
                           rates,dc, bdc, redemption, issueDate);
        
        
        
        RcppResultSet rs;
        rs.add("yield", bond.yield(price, dc, cp, freq));
        rl = rs.getReturnList();
        
    } catch(std::exception& ex) {
        exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
        exceptionMesg = copyMessageToR("unknown reason");
    }
    
    if(exceptionMesg != NULL)
        error(exceptionMesg);
    
    return rl;
}
 
RcppExport  SEXP QL_FixedRateBondPriceByYield(SEXP optionParameters, SEXP ratesVec) {
  
    SEXP rl=R_NilValue;
    char* exceptionMesg=NULL;
    try{
        RcppParams rparam(optionParameters);
        double settlementDays = rparam.getDoubleValue("settlementDays");
        std::string cal = rparam.getStringValue("calendar");
        double yield = rparam.getDoubleValue("yield");
        double faceAmount = rparam.getDoubleValue("faceAmount");
        double businessDayConvention = rparam.getDoubleValue("businessDayConvention");
        double compound = rparam.getDoubleValue("compound");
        double redemption = rparam.getDoubleValue("redemption");
        double dayCounter = rparam.getDoubleValue("dayCounter");
        double frequency = rparam.getDoubleValue("period");
        
        RcppDate mDate = rparam.getDateValue("maturityDate");
        RcppDate eDate = rparam.getDateValue("effectiveDate");
        RcppDate iDate = rparam.getDateValue("issueDate");
        QuantLib::Date maturityDate(dateFromR(mDate));
        QuantLib::Date effectiveDate(dateFromR(eDate));
        QuantLib::Date issueDate(dateFromR(iDate));
        
        //extract coupon rates vector
        RcppVector<double> RcppVec(ratesVec); 
        std::vector<double> rates(RcppVec.stlVector());
        
        //set up BusinessDayConvetion
        BusinessDayConvention bdc = getBusinessDayConvention(businessDayConvention);
        DayCounter dc = getDayCounter(dayCounter);
        Frequency freq = getFrequency(frequency);
        Compounding cp = getCompounding(compound);
 
        //set up calendar
        Calendar calendar = UnitedStates(UnitedStates::GovernmentBond);
        if (cal == "us"){
            calendar = UnitedStates(UnitedStates::GovernmentBond);
        }
        else if (cal == "uk"){
            calendar = UnitedKingdom(UnitedKingdom::Exchange);
        }
        
        //build the bond
        Schedule sch(effectiveDate, maturityDate,
                     Period(freq), calendar,
                     bdc, bdc, DateGeneration::Backward, false);
        
        FixedRateBond bond(settlementDays, faceAmount, sch,
                           rates,dc, bdc, redemption, issueDate);
        
        
        
        RcppResultSet rs;
        rs.add("cleanPrice", bond.cleanPrice(yield, dc, cp, freq));
        rl = rs.getReturnList();
        
    } catch(std::exception& ex) {
        exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
        exceptionMesg = copyMessageToR("unknown reason");
    }
    
    if(exceptionMesg != NULL)
        error(exceptionMesg);
    
    return rl;
}


RcppExport  SEXP QL_FloatingRateBond(SEXP optionParameters, SEXP sch, SEXP index,
                                     SEXP gearingsVec, SEXP spreadsVec,
                                     SEXP capsVec, SEXP floorsVec) {
  
    SEXP rl=R_NilValue;
    char* exceptionMesg=NULL;
    try{
        //extract optionParameters
        RcppParams rparam(optionParameters);
        double settlementDays = rparam.getDoubleValue("settlementDays");
        double faceAmount = rparam.getDoubleValue("faceAmount");
        double businessDayConvention = rparam.getDoubleValue("businessDayConvention");
        double fixingDays = rparam.getDoubleValue("fixingDays");
        double redemption = rparam.getDoubleValue("redemption");
        double dayCounter = rparam.getDoubleValue("dayCounter");
        Rate riskFreeRate = rparam.getDoubleValue("riskFreeRate");
        
        RcppDate iDate = rparam.getDateValue("issueDate");
        RcppDate today_Date = rparam.getDateValue("todayDate");       
        QuantLib::Date issueDate(dateFromR(iDate));
        QuantLib::Date today(dateFromR(today_Date));
        
        //extract schedule
        Schedule schedule = getSchedule(sch);
        //extract index
        boost::shared_ptr<IborIndex> iindex = getIborIndex(index, today);

        //extract gearings, spreads, caps, and floors
        std::vector<double> gearings = getDoubleVector(gearingsVec);
        std::vector<double> spreads = getDoubleVector(spreadsVec);
        std::vector<double> caps = getDoubleVector(capsVec);
        std::vector<double> floors = getDoubleVector(floorsVec);
                                       
        //set up BusinessDayConvetion
        BusinessDayConvention bdc = getBusinessDayConvention(businessDayConvention);
        DayCounter dc = getDayCounter(dayCounter);
        
        //build the curve
        boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(riskFreeRate));
        Settings::instance().evaluationDate() = today;
        Handle<YieldTermStructure> discountCurve(flatRate(today,rRate,Actual360()));
        
        //build the bond
        FloatingRateBond bond(settlementDays, faceAmount, schedule,
                              iindex, dc, bdc, fixingDays,
                              gearings, spreads, caps, floors, false,
                              redemption, issueDate);        
        
        
        //bond price
        boost::shared_ptr<PricingEngine> bondEngine(
                                                    new DiscountingBondEngine(discountCurve));
        bond.setPricingEngine(bondEngine);



        
        //cashflow
        boost::shared_ptr<IborCouponPricer> pricer(new
                                                   BlackIborCouponPricer(Handle<OptionletVolatilityStructure>()));
        setCouponPricer(bond.cashflows(),pricer);

        int numCol = 2;
        std::vector<std::string> colNames(numCol);
        colNames[0] = "Date";
        colNames[1] = "Amount";
        RcppFrame frame(colNames);
        
        Leg bondCashFlow = bond.cashflows();
        for (unsigned int i = 0; i< bondCashFlow.size(); i++){
            std::vector<ColDatum> row(numCol);
            Date d = bondCashFlow[i]->date();
            row[0].setDateValue(RcppDate(d.month(), d.dayOfMonth(), d.year()));
            row[1].setDoubleValue(bondCashFlow[i]->amount());
            frame.addRow(row);
        }
        
        
        RcppResultSet rs;

        rs.add("NPV", bond.NPV());
        rs.add("cleanPrice", bond.cleanPrice());
        rs.add("dirtyPrice", bond.dirtyPrice());
        rs.add("accruedCoupon", bond.accruedAmount());
        rs.add("yield", bond.yield(Actual360(), Compounded, Annual));
        rs.add("cashFlow", frame);
        rl = rs.getReturnList();
        
    } catch(std::exception& ex) {
        exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
        exceptionMesg = copyMessageToR("unknown reason");
    }
    
    if(exceptionMesg != NULL)
        error(exceptionMesg);
    
    return rl;
}
