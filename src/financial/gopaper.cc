/* this is --*- C++ -*-- */

#include <gotypes.h>
#include <gopaper.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>

#ifndef __GOPAPER_NAMES_DEFINED__
#define __GOPAPER_NAMES_DEFINED__
/*
 * names for paper output fields
 */
const char* GOP_TICKER    = "ticker";
const char* GOP_NAME      = "name";
const char* GOP_NR        = "nr";
const char* GOP_AMOUNT    = "amount";
const char* GOP_BUYPRICE  = "buyprice";
const char* GOP_SELLPRICE = "sellprice";
const char* GOP_COSTS     = "costs";
const char* GOP_SOLD      = "sold";
const char* GOP_BUYDATE   = "buydate";
const char* GOP_SELLDATE  = "selldate";
const char* GOP_CURRENCY  = "currency";
const char* GOP_FOREIGN	  = "foreigncurrency";
const char* GOP_TIMEOUT   = "timeout";
const char* GOP_INTEREST  = "interest";
const char* GOP_PREPAID   = "prepaid";
const char* GOP_XCHGBUY   = "xchgbuy";
const char* GOP_XCHGSELL  = "xchgsell";
const char* GOP_TYPE	  = "type";

const char* GOP_SECTION_START = "start";
const char* GOP_SECTION_END = "end";
#endif /* __GOPAPER_NAMES_DEFINED__ */

/* Constructors */
goPaper::
goPaper() {
  tickerSymbol.resize (4);
  tickerSymbol = "NONE";
  name.resize (4);
  name 		= "NONE";
  nr.resize (4);
  nr 		= "NONE";
  amount 	= 0;
  buyPrice 	= 0;
  sellPrice 	= 0;
  costs 	= 0;
  sold 		= false;
  buyDate.setDay  (1);
  buyDate.setMonth(1);
  buyDate.setYear (1900);
  currency.resize (3);
  currency 	   = "DEM";
  foreignCurrency  = "DEM";
  timeOut.setDay (0);
  timeOut.setMonth (0);
  timeOut.setYear (0000);
  interest 	   = 0.0;
  prePaid	   = 0.0;
  exchangeRateBuy  = 1;
  exchangeRateSell = 1;
  type		   = SHARE;
}

goPaper::
goPaper (goPaper& other) {
  tickerSymbol  = other.getTicker();
  name 		= other.getName();
  nr 		= other.getNr();
  amount 	= other.getAmount();
  buyPrice 	= other.getBuyPrice();
  sellPrice 	= other.getSellPrice();
  costs 	= other.getCosts();
  sold 		= other.isSold();
  buyDate 	= other.getBuyDate();
  sellDate 	= other.getSellDate();
  currency 	= other.getCurrency();

  foreignCurrency  = other.getForeignCurrency ();
  timeOut	   = other.getTimeOut ();
  interest	   = other.getInterest();
  prePaid	   = other.getPrePaid();
  exchangeRateBuy  = other.getExchangeRateBuy();
  exchangeRateSell = other.getExchangeRateSell();

  type		   = other.getType();
}

goPaper::
goPaper(goString& ticker,
	goString& nam,
	goString& n,
	goString& curr,
	goInt32   am,
	goFloat   buyP,
	goFloat   sellP,
	goFloat   cost,
	bool 	  s,
	goDate&   buyDat,
	goDate&   sellDat) {

  tickerSymbol = ticker;
  name = nam;
  nr = n;
  amount = am;
  buyPrice = buyP;
  sellPrice = sellP;
  costs = cost;
  sold = s;
  buyDate = buyDat;
  sellDate = sellDat;
  currency = curr;
}

inline
goDate&
goPaper::
getBuyDate(void) const {
  return (goDate&)buyDate;
}

inline
goDate&
goPaper::
getSellDate(void) const {
  return (goDate&)sellDate;
}

void
goPaper::
setBuyDate(goDate& d) {
  buyDate = (goDate&)d;
}

void 
goPaper::
setSellDate(goDate& d) {
  sellDate = d;
}

ostream&
operator<< (ostream& outstream,goPaper& paper) {
  char str[255];
  outstream << GOP_TICKER    << " = " << paper.getTicker  () << "\n"; 
  outstream << GOP_NAME      << " = " << paper.getName    () << "\n"; 
  outstream << GOP_NR        << " = " << paper.getNr      () << "\n"; 
  outstream << GOP_CURRENCY  << " = " << paper.getCurrency() << "\n"; 
  outstream << GOP_AMOUNT    << " = " << paper.getAmount  () << "\n"; 
  sprintf(str,"%.2f",paper.getBuyPrice());
  outstream << GOP_BUYPRICE  << " = " << str << "\n"; 
  sprintf(str,"%.2f",paper.getSellPrice());
  outstream << GOP_SELLPRICE << " = " << str << "\n"; 
  sprintf(str,"%.2f",paper.getCosts());
  outstream << GOP_COSTS     << " = " << str << "\n"; 
  outstream << GOP_SOLD      << " = " << paper.isSold     () << "\n"; 
  outstream << GOP_BUYDATE   << " = " << paper.getBuyDate () << "\n"; 
  outstream << GOP_SELLDATE  << " = " << paper.getSellDate() << "\n"; 
  outstream << GOP_FOREIGN   << " = " << paper.getForeignCurrency() << "\n";
  outstream << GOP_TIMEOUT   << " = " << paper.getTimeOut() << "\n";
  outstream << GOP_INTEREST  << " = " << paper.getInterest() << "\n";
  outstream << GOP_PREPAID   << " = " << paper.getPrePaid() << "\n";
  outstream << GOP_XCHGBUY   << " = " << paper.getExchangeRateBuy() << "\n";
  outstream << GOP_XCHGSELL  << " = " << paper.getExchangeRateSell() << "\n";
  outstream << GOP_TYPE	     << " = ";
  if (paper.type == goPaper::SHARE) { outstream << "share\n";}
  else {
    if (paper.type == goPaper::LOAN) { outstream << "loan\n";}
    else { 
      outstream << "unknown\n";
    }
  }
  return outstream;
}

/*
ostream&
operator<< (ostream& outstream,const goPaper& paper) {
  char str[255];
  outstream << GOP_TICKER    << " = " << paper.getTicker  () << "\n"; 
  outstream << GOP_NAME      << " = " << paper.getName    () << "\n"; 
  outstream << GOP_NR        << " = " << paper.getNr      () << "\n"; 
  outstream << GOP_CURRENCY  << " = " << paper.getCurrency() << "\n"; 
  outstream << GOP_AMOUNT    << " = " << paper.getAmount  () << "\n"; 
  sprintf(str,"%.2f",paper.getBuyPrice());
  outstream << GOP_BUYPRICE  << " = " << str << "\n"; 
  sprintf(str,"%.2f",paper.getSellPrice());
  outstream << GOP_SELLPRICE << " = " << str << "\n"; 
  sprintf(str,"%.2f",paper.getCosts());
  outstream << GOP_COSTS     << " = " << str << "\n"; 
  outstream << GOP_SOLD      << " = " << paper.isSold     () << "\n"; 
  outstream << GOP_BUYDATE   << " = " << paper.getBuyDate () << "\n"; 
  outstream << GOP_SELLDATE  << " = " << paper.getSellDate() << "\n"; 
  outstream << GOP_FOREIGN   << " = " << paper.getForeignCurrency() << "\n";
  outstream << GOP_TIMEOUT   << " = " << paper.getTimeOut() << "\n";
  outstream << GOP_INTEREST  << " = " << paper.getInterest() << "\n";
  outstream << GOP_PREPAID   << " = " << paper.getPrePaid() << "\n";
  outstream << GOP_XCHGBUY   << " = " << paper.getExchangeRateBuy() << "\n";
  outstream << GOP_XCHGSELL  << " = " << paper.getExchangeRateSell() << "\n";
  outstream << GOP_TYPE	     << " = ";
  if (paper.type == goPaper::SHARE) { outstream << "share\n";}
  else {
    if (paper.type == goPaper::LOAN) { outstream << "loan\n";}
    else { 
      outstream << "unknown\n";
    }	
  }
  return outstream;
}
*/

goPaper&
goPaper::operator= (goPaper& other) {
  setTicker 		(other.getTicker());
  setName   		(other.getName());
  setNr    		(other.getNr());
  setAmount	 	(other.getAmount());
  setBuyPrice 		(other.getBuyPrice());
  setSellPrice 		(other.getSellPrice());
  setCosts 	 	(other.getCosts());
  setSold 		(other.isSold());
  setCurrency 		(other.getCurrency());
  setForeignCurrency 	(other.getForeignCurrency());
  setTimeOut		(other.getTimeOut());
  setInterest		(other.getInterest());
  setPrePaid		(other.getPrePaid());
  setExchangeRateBuy  	(other.getExchangeRateBuy());
  setExchangeRateSell 	(other.getExchangeRateSell());
  setType		(other.getType());
  return (*this);
}

/*
goPaper&
goPaper::operator= (const goPaper& other) {
  setTicker 	(other.getTicker());
  setName   	(other.getName());
  setNr    	(other.getNr());
  setAmount 	(other.getAmount());
  setBuyPrice 	(other.getBuyPrice());
  setSellPrice 	(other.getSellPrice());
  setCosts  	(other.getCosts());
  setSold 	(other.isSold());
  setCurrency 	(other.getCurrency());
  setForeignCurrency 	(other.getForeignCurrency());
  setTimeOut		(other.getTimeOut());
  setInterest		(other.getInterest());
  setPrePaid		(other.getPrePaid());
  setExchangeRateBuy  	(other.getExchangeRateBuy());
  setExchangeRateSell 	(other.getExchangeRateSell());
  setType		(other.getType());
  return (*this);
}
*/

goFloat
goPaper::
getExchangeGain () {
  goFloat exchangeGain = 0;

  exchangeGain = (exchangeRateSell * getSellPrice() * getAmount()) -
                 (exchangeRateBuy * getBuyPrice() * getAmount());
  return exchangeGain;
}

goFloat
goPaper::
getGain () {
  goFloat totalSold = 0;
  goFloat totalCost = 0;

  totalSold += getSellPrice() * getAmount();
  totalCost += getBuyPrice()  * getAmount();
  if (!(currency == foreignCurrency)) {
    totalSold *= (exchangeRateSell);
    totalCost *= (exchangeRateBuy);
  }
  totalCost += getCosts();

  return (totalSold - totalCost);
}

goFloat
goPaper::
getValue () {
  goFloat value = 0;
  if (!isSold()) {
    value  = (goFloat) (getSellPrice() * getAmount());
    if (!(currency == foreignCurrency)) {
      value *= (exchangeRateSell);
    }
  }
  return value;
}










