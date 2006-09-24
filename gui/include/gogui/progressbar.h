#ifndef GOGUI_PROGRESSBAR_H
#define GOGUI_PROGRESSBAR_H

namespace goGUI
{
    /** 
     * @brief Gtk::ProgressBar with an internal fraction step and fraction stored.
     * incrementFraction() increments the current fraction and update()
     * simply does this->set_fraction(this->get_fraction()).
     */
    class ProgressBar : public Gtk::ProgressBar
    {
        public:
            ProgressBar ();
            virtual ~ProgressBar ();

            void setFractionStep (goDouble s);
            goDouble getFractionStep () const;

            void incrementFraction ();
            void update ();

        private:
            goDouble myFractionStep;
            goDouble myFraction;
    };
};

#endif
