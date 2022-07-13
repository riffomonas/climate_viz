#.libPaths(c(.libPaths()[2], .libPaths()[1]))
env_index <- grep("/envs/", .libPaths())
.libPaths(c(.libPaths()[env_index],.libPaths()[-env_index]))
