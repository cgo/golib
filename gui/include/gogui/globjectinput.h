#ifndef GOGUI_GLOBJECTINPUT_H
#define GOGUI_GLOBJECTINPUT_H

#include <gogui/multivectorinput.h>
#include <gogl/object.h>
#include <gtkmm.h>
#ifndef GOAUTOPTR_H
# include <goautoptr.h>
#endif

namespace goGUI
{
    class GLObjectInputPrivate;

    class GLObjectInput : public Gtk::Frame
    {
        public:
            GLObjectInput ();
            virtual ~GLObjectInput ();

            void setObject (goAutoPtr<goGL::Object> o);
            goAutoPtr<goGL::Object> getObject ();

            virtual void updateInput ();

            sigc::signal<void>& signalObjectInputChanged ();

        protected:
            void set (const goGL::Object& o);
            void get (goGL::Object& o);
            void inputChangedSlotObject ();
            Gtk::Box* getBox ();

        private:
            GLObjectInput (const GLObjectInput& o);
            GLObjectInput& operator= (const GLObjectInput&);

        private:
            GLObjectInputPrivate* myPrivate;
    };

    class GLObjectVectorInput : public MultiVectorInput
    {
        public:
            GLObjectVectorInput ();
            virtual ~GLObjectVectorInput ();
            
            void set (const goGL::Object& o);
            void get (goGL::Object& o);
            void inputChangedSlotObject ();
            sigc::signal<void>& signalObjectInputChanged ();

        private:
            GLObjectVectorInput (const GLObjectVectorInput&);
            GLObjectVectorInput& operator= (const GLObjectVectorInput&);

        private:
            sigc::signal<void> myInputChangedSignal;
    };
};

#endif

