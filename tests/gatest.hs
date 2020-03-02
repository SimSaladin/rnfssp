------------------------------------------------------------------------------
-- File:          
-- Creation Date:
-- Last Modified: Sep 15 2013 [02:40:33]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

import Sections.BackendGitAnnex
import Data.Conduit
import Data.Conduit.List as CL

main = gitSource "/home/media/anime" ["annex", "find"] $$ CL.mapM_ print
