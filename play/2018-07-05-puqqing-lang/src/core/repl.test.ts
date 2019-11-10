// Run unit tests of modules.
// Test codes are exported by each module.

import { testSuite as replTests } from './repl';

describe('index', () => replTests());
