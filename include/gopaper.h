/* --*- C++ -*-- */
#ifndef GOPAPER_H
#define GOPAPER_H

#endif

#ifndef __GOPAPER_H__
#define __GOPAPER_H__

#define GOP_NUMBER_OF_ENTRIES 17

#include <gotypes.h>
#include <godate.h>
#include <gostring.h>

/**
 * Provides stock paper handling.
 * @author: Christian Gosch
 */
class 
goPaper {
public:
  ///
  goPaper ();
  ///
  goPaper (goPaper& other);
  ///
  goPaper (goString& ticker,	  /* DO NOT use this ! */
	   goString& name,
	   goString& nr,
	   goString& curr,
	   goInt32 am,
	   goFloat buyP,
	   goFloat sellP,
	   goFloat costs,
	   bool s,
	   goDate& buyDat,
	   goDate& sellDat);

  ///
  enum {
    SHARE,
    LOAN,
    UNKNOWN
  };

  ///
  friend std::ostream& operator<< (std::ostream& outstream,
			      goPaper& paper);
  /*  friend ostream& operator<< (ostream& outstream,
			      const goPaper& paper);
  */
  goPaper&        operator=  (goPaper& other);
  /*  goPaper&        operator=  (const goPaper& other); */

  ///
  void 		setTicker (goString& ticker) { 
    tickerSymbol = ticker;
  }  
  ///
  void 		setTicker (const char* ticker) { 
    tickerSymbol = ticker;
  }  
  ///
  void 		setName   (goString& nam) { 
    name = nam;
  }
  ///
  void		setName	  (const char* nam) {
    name = nam;
  }
  ///
  void 		setNr     (goString& n) { 
    nr = n;
  }
  ///
  void 		setNr     (const char* n) { 
    nr = n;
  }
  ///
  void 		setAmount 	(goInt32 a) {amount = a;}
  ///
  void 		setBuyPrice 	(goFloat p) {buyPrice = p;}
  ///
  void 		setSellPrice 	(goFloat p) {sellPrice = p;}
  ///
  void 		setCosts 	(goFloat p) {costs = p;}
  ///
  bool 		isSold 		(void) const {return sold;}
  ///
  void 		setSold 	(bool s) {sold = s;}
  ///
  void 		setBuyDate	(goDate& d);
  ///
  void 		setSellDate	(goDate& d);
  ///
  void		setCurrency 	(goString& c) { currency = c; }
  ///
  void		setCurrency 	(const char* c) { currency = c; }
  ///
  void		setForeignCurrency (goString& s) 
    { foreignCurrency = s; }
  ///
  void		setForeignCurrency (const char* s) 
    { foreignCurrency = s; }
  ///
  void		setTimeOut	(goDate& d) 
    { timeOut = d; }
  ///
  //void		setTimeOut	(const goDate& d) 
  //  { timeOut = d; }
  ///
  void		setInterest	(goFloat p) 
    { interest = p; }
  ///
  void 		setPrePaid	(goFloat p) 
    { prePaid = p; }
  ///
  void 		setExchangeRateBuy (goFloat p) 
    { exchangeRateBuy = p; }
  ///
  void 		setExchangeRateSell (goFloat p) 
    { exchangeRateSell = p; }

  ///
  void 		setType ( int t ) { type = t; }
  
  ///
  inline goDate& 	getBuyDate(void) const;
  ///
  inline goDate&	getSellDate(void) const;
  ///
  goInt32 		getAmount() const {return amount;}
  ///
  goFloat 		getBuyPrice() const {return buyPrice;}
  ///
  goFloat 		getSellPrice() const {return sellPrice;}
  ///
  goFloat 		getCosts() const {return costs;}
  ///
  goString& 	getTicker() {return tickerSymbol;}
  ///
  goString& 	getName() {return name;}
  ///
  goString& 	getNr() {return nr;}
  ///
  goString& 	getCurrency() {return currency;}
  ///
  goString& 	getForeignCurrency() {return foreignCurrency;}
  ///
  goDate&		getTimeOut () { return timeOut; }
  ///
  goFloat		getInterest () { return interest; } 
  ///
  goFloat		getPrePaid () { return prePaid; } 
  ///
  goFloat		getExchangeRateBuy () { return exchangeRateBuy; } 
  ///
  goFloat		getExchangeRateSell () { return exchangeRateSell; } 

  ///
  int			getType () { return type; }

  /** Calculation stuff */
  //
  ///
  goFloat		getValue ();
  ///
  goFloat		getGain  ();
  ///
  goFloat		getExchangeGain ();
  //

 protected:
  ///
  goString 	tickerSymbol;
  ///
  goString 	name;
  ///
  goString 	nr;
  ///
  goString 	currency;
  ///
  goInt32 	amount;
  ///
  goFloat 	buyPrice;
  ///
  goFloat 	sellPrice;
  ///
  goFloat 	costs;
  ///
  bool 		sold;
  ///
  goDate 	buyDate;
  ///
  goDate 	sellDate;

  /// if currency is not the same as yours
  goString      foreignCurrency;	
  /// date the paper has to be sold
  goDate 	timeOut;		
  ///
  goFloat	interest;		
  /// money you paid and get back when you sell	
  goFloat	prePaid;		
  /// x-change rate at buytime	
  goFloat	exchangeRateBuy;	
  /// x-change rate at selltime
  goFloat	exchangeRateSell;	

  ///
  int		type;
};

#endif











