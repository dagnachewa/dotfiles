# Changes

+ **added boxdraw patch** to make lines gapless
+ **Shift-Mouse Wheel** now scrolls five lines at a time
+ **changed cursor's color and shape** to make it stand out more (.Xresources)

I also plan on adding delkey patch in the near future.

The .Xresources file I included, contains a few color schemes and some minor changes like cursor color and transparency. 
Copy the contents of the file starting from "!! st configuration" to .Xdefaults in your home directory.
Rename the file to .Xresources and run the following command to create a symlink: 
```ln -s .Xresources .Xdefaults```

## Installation 

```
git clone https://gitlab.com/Matii/st.git
cd st
sudo make install
```

## Contact
+ miekiv@protonmail.com

**For more information, check out [Luke Smith's Repo](https://github.com/LukeSmithxyz/st) on which my fork is based**

Enjoy!
