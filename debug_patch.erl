% This is a debug patch to understand the link resolution issue
% The problem seems to be that when a nested message is stored:
% 1. do_write_message returns the UncommittedID (message ID)
% 2. A group is created at that ID
% 3. Links are created from the ID to each key's hashpath
% 4. When reading back, lazy links are returned
% 5. ensure_loaded tries to read the linked ID but fails

% The issue might be:
% - The store parameter in the link is not being used correctly
% - The message ID format doesn't match what the store expects
% - The group creation or link resolution has a bug

% Potential fixes:
% 1. Ensure the store parameter is correctly passed through link resolution
% 2. Check if the message ID needs to be in a specific format
% 3. Verify that groups are created correctly and can be read back