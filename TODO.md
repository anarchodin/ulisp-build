# Unsorted list of tasks that should be done

 -  __Handle the inclusion of user-visible functions better.__

    The current system is more painful to work with than it needs to be. There
    are groups of functions that will always be included together; they should
    share a file.

    Doing this will presumably require a change to how `*definitions*`
    represents the sections. It might be worth simply attaching a filename to
    the section and adding a string to identify the C function to call.
