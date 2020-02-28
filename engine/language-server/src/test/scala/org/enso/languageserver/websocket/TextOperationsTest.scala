package org.enso.languageserver.websocket

import io.circe.literal._

class TextOperationsTest extends WebSocketServerTest {
  "text/openFile" must {
    "fail opening a file if it does not exist" in {
      // Interaction:
      // 1. Client tries to open a non-existent file.
      // 2. Client receives an error message.
      val client = new WsTestClient(address)

      // 1
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)

      // 2
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "error": { "code": 1003, "message": "File not found" }
          }
          """)
    }

    "allow opening files for editing" in {
      // Interaction:
      // 1. Client creates a file.
      // 2. Client receives confirmation.
      // 3. Client opens the created file.
      // 4. Client receives the file contents and a canEdit capability.
      val client = new WsTestClient(address)

      // 1
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)

      // 2
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": null
          }
          """)

      // 3
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)

      // 4
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": {
              "writeCapability": {
                "id": ${idGenerator.capabilityRegistrations(0)},
                "method": "canEdit",
                "registerOptions": { "path": {
                  "rootId": $testContentRootId,
                  "segments": ["foo.txt"]
                } }
              },
              "content": "123456789",
              "currentVersion": ${idGenerator.bufferVersions(0)}
            }
          }
          """)
    }

    "allow opening files for reading for another client" in {
      // Interaction:
      // 1. Client 1 creates a file.
      // 2. Client 1 receives confirmation.
      // 3. Client 1 opens the created file.
      // 4. Client 1 receives the file contents and a canEdit capability.
      // 5. Client 2 opens the file.
      // 6. Client 2 receives the file contents without a canEdit capability.
      val client1 = new WsTestClient(address)
      val client2 = new WsTestClient(address)

      // 1
      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)

      // 2
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": null
          }
          """)

      // 3
      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)

      // 4
      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": {
              "writeCapability": {
                "id": ${idGenerator.capabilityRegistrations(0)},
                "method": "canEdit",
                "registerOptions": { "path": {
                  "rootId": $testContentRootId,
                  "segments": ["foo.txt"]
                } }
              },
              "content": "123456789",
              "currentVersion": ${idGenerator.bufferVersions(0)}
            }
          }
          """)

      // 5
      client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 2,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)

      // 6
      client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": {
              "writeCapability": null,
              "content": "123456789",
              "currentVersion": ${idGenerator.bufferVersions(0)}
            }
          }
          """)
    }
  }

  "grant the canEdit capability if no one else holds it" in {
    // Interaction:
    // 1. Client 1 creates a file.
    // 2. Client 1 receives confirmation.
    // 3. Client 1 opens the created file.
    // 4. Client 1 receives the file contents and a canEdit capability.
    // 5. Client 1 releases the canEdit capability.
    // 6. Client 1 receives a confirmation.
    // 7. Client 2 opens the file.
    // 8. Client 2 receives the file contents and a canEdit capability.
    val client1 = new WsTestClient(address)
    val client2 = new WsTestClient(address)

    // 1
    client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 0,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)

    // 2
    client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 0,
            "result": null
          }
          """)

    // 3
    client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)

    // 4
    client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": {
              "writeCapability": {
                "id": ${idGenerator.capabilityRegistrations(0)},
                "method": "canEdit",
                "registerOptions": { "path": {
                  "rootId": $testContentRootId,
                  "segments": ["foo.txt"]
                } }
              },
              "content": "123456789",
              "currentVersion": ${idGenerator.bufferVersions(0)}
            }
          }
          """)

    // 5
    client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/release",
            "id": 2,
            "params": {
              "id": ${idGenerator.capabilityRegistrations(0)}
            }
          }
          """)

    // 6
    client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)

    // 7
    client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "text/openFile",
            "id": 3,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)

    // 8
    client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 3,
            "result": {
              "writeCapability": {
                "id": ${idGenerator.capabilityRegistrations(1)},
                "method": "canEdit",
                "registerOptions": { "path": {
                  "rootId": $testContentRootId,
                  "segments": ["foo.txt"]
                } }
              },
              "content": "123456789",
              "currentVersion": ${idGenerator.bufferVersions(0)}
            }
          }
          """)
  }
}
