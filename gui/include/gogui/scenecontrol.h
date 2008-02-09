#ifndef GOGUI_SCENECONTROL_H
#define GOGUI_SCENECONTROL_H

#include <gogui/control.h>
#include <gogui/sceneview.h>

namespace goGUI
{
    class SceneControlPrivate;

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

            void addWaypoint ();
            void removeWaypoint ();
            void prependWaypoint ();
            void appendWaypoint ();
            int  animationPositionChanged ();
            void createMovie ();

            void editWaypointToggled ();
            void editWaypoint ();
            
            int transformToSelectedWaypoint ();

        private:
            SceneControl (const SceneControl&);
            SceneControl& operator= (const SceneControl& o);

        private:
            SceneControlPrivate* myPrivate;
    };
};

#endif
