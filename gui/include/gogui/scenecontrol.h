/* Copyright (C) 1998-2011 Christian Gosch, golib at goschs dot de
   This file is part of the golib library.
   For license regulations, see the file COPYING in the main
   directory of the golib source tree. */


#ifndef GOGUI_SCENECONTROL_H
#define GOGUI_SCENECONTROL_H

#include <gogui/control.h>
#include <gogui/sceneview.h>

namespace goGUI
{
    class SceneControlPrivate;

/** @addtogroup gui
 * @{
 */
    /** 
     * @brief Control for a \c goGUI::SceneView.
     */
    class SceneControl : public Control
    {
        public:
            SceneControl ();
            virtual ~SceneControl ();

            goGUI::SceneView& getSceneView ();

            void loadOFF ();
            void deleteObject ();
            void loadImage ();
            void onHide ();
            void onShow ();

            void objectBoxChangedSlot ();
            void lightBoxChangedSlot ();

            int objectPropChanged ();
            void objectProperties ();

            void viewChanged ();

            void saveScene ();
            void loadScene ();
            void saveImage ();

            void clearColourChanged ();
            void ambientChanged ();

            void addWaypoint ();
            void removeWaypoint ();
            void prependWaypoint ();
            void appendWaypoint ();
            int  animationPositionChanged ();
            void createMovie ();
            void constantSpeedToggled ();

            void editWaypointToggled ();
            void editWaypoint ();
            
            int transformToSelectedWaypoint ();

        private:
            SceneControl (const SceneControl&);
            SceneControl& operator= (const SceneControl& o);

        private:
            SceneControlPrivate* myPrivate;
    };
/** 
 * @}
 */
};

#endif
